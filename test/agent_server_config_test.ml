open Core

let () = Mirage_crypto_rng_unix.use_default ()

let with_fixture f =
  Eio_main.run (fun env ->
    let base = Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp" in
    let temporary =
      Filename.concat
        base
        ("ochat-agent-server-config."
         ^ (Agent_protocol.Id.Transaction.create ()
            |> Agent_protocol.Id.Transaction.to_string))
    in
    let root = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat temporary "workspace" in
        let prompt = Filename.concat temporary "agent.chatmd" in
        let token_file = Filename.concat temporary "tokens.sexp" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>You are a test agent.</developer>";
        let digest =
          Digestif.SHA256.digest_string "test-token" |> Digestif.SHA256.to_hex
        in
        let principal = Agent_protocol.Id.Principal.create () in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / token_file)
          (sprintf
             "(((token_sha256 %s) (principal_id %s) (scopes (diagnostics.read)) \
              (attributes ()) (expires_at none)))"
             digest
             (Agent_protocol.Id.Principal.to_string principal));
        f env temporary workspace prompt token_file)
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root))
;;

let config_text ~workspace ~prompt ~http =
  sprintf
    {|
(version 1)
(server
 ((data_dir "./data")
  (unix_socket "./agent.sock")
  (http %s)
  (shutdown_grace_ms 30000)
  (event_retention ((completed_stream_ms 3600000) (max_events_per_session 100000)))
  (durability
   ((journal_flush interval)
    (journal_flush_ms 50)
    (snapshot_every_events 100)
    (snapshot_every_ms 5000)))))
(workspaces
 (((id project)
   (source (physical "%s"))
   (access shared_write)
   (prompt_limits (((prompt coding-agent) (max_root_agents 2) (overflow queue)))))))
(prompts
 (((id coding-agent)
   (path "%s")
   (description "Coding agent")
   (allowed_workspaces (project))
   (permission_profile interactive)
   (enabled true))))
(permission_profiles
 (((id interactive)
   (tool_default ask)
   (approval_timeout none)
   (approval_fallback deny)
   (manifest_authorization require_grant))))
|}
    http
    workspace
    prompt
;;

let validate env source_file contents =
  let raw =
    Agent_server.Config_parser.parse_string ~source_file contents
    |> function
    | Ok value -> value
    | Error diagnostics ->
      raise_s
        [%sexp
          "unexpected parse diagnostics"
        , (diagnostics : Agent_server.Config.Diagnostic.t list)]
  in
  Agent_server.Config_validator.validate ~env raw
;;

let validated_exn env source_file contents =
  match validate env source_file contents with
  | Ok value -> value
  | Error diagnostics ->
    raise_s
      [%sexp
        "unexpected validation diagnostics"
      , (diagnostics : Agent_server.Config.Diagnostic.t list)]
;;

let%test_unit "prompt compiler contract is explicit and defaults to Standard" =
  with_fixture (fun env temporary workspace prompt _ ->
    let source_file = Filename.concat temporary "server.sexp" in
    let base = config_text ~workspace ~prompt ~http:"((enabled false))" in
    let contract text =
      (List.hd_exn (validated_exn env source_file text).prompts).tool_contract
    in
    [%test_eq: Chatml_compilation.Tool_contract.t]
      (contract base)
      Chatml_compilation.Tool_contract.Standard;
    let developer =
      String.substr_replace_first
        base
        ~pattern:"(enabled true)"
        ~with_:"(tool_contract developer_model_spawn) (enabled true)"
    in
    [%test_eq: Chatml_compilation.Tool_contract.t]
      (contract developer)
      Chatml_compilation.Tool_contract.Developer_model_spawn;
    let unknown =
      String.substr_replace_first
        base
        ~pattern:"(enabled true)"
        ~with_:"(tool_contract unknown_contract) (enabled true)"
    in
    match validate env source_file unknown with
    | Ok _ -> assert false
    | Error diagnostics ->
      assert (
        List.exists diagnostics ~f:(fun diagnostic ->
          String.is_substring diagnostic.config_path ~substring:"tool_contract")))
;;

let%expect_test "authoring budgets validate as a captured server configuration" =
  with_fixture (fun env temporary workspace prompt _ ->
    let source_file = Filename.concat temporary "server.sexp" in
    let text budget =
      String.substr_replace_first
        (config_text ~workspace ~prompt ~http:"((enabled false))")
        ~pattern:"(shutdown_grace_ms 30000)"
        ~with_:("(authoring_budget (" ^ budget ^ "))")
    in
    let configured =
      validated_exn
        env
        source_file
        (text "(default_tokens 6000) (max_tokens 8000) (preload_tokens 16000)")
    in
    let budget = Option.value_exn configured.server.authoring_budget in
    print_s [%sexp (budget : Chat_response.Authoring_validation.context_budget)];
    let defaults = validated_exn env source_file (text "") in
    assert (
      Chat_response.Authoring_validation.equal_context_budget
        (Option.value_exn defaults.server.authoring_budget)
        Chat_response.Authoring_validation.default_context_budget);
    List.iter
      [ "(default_tokens 9000) (max_tokens 8000)"
      ; "(preload_tokens 0)"
      ; "(max_tokens 1000001)"
      ; "(default_tokens 6000) (default_tokens 7000)"
      ; "(extra_budget 42)"
      ]
      ~f:(fun invalid ->
        assert (Result.is_error (validate env source_file (text invalid))));
    print_endline
      "defaults captured; reversed, zero, oversized, duplicate and unknown settings \
       rejected");
  [%expect
    {|
    ((default_tokens 6000) (max_tokens 8000) (preload_tokens 16000))
    defaults captured; reversed, zero, oversized, duplicate and unknown settings rejected
    |}]
;;

let%expect_test
    "helper configuration pins its public boundary and rejects credential exposure"
  =
  with_fixture (fun env temporary workspace prompt token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let helper = Filename.concat workspace "helper" in
    Eio.Path.save
      ~create:(`Exclusive 0o700)
      Eio.Path.(Eio.Stdenv.fs env / helper)
      "fixture";
    let sha = Chatmd_shell_spec.Source_ref.digest "fixture" in
    let policy =
      sprintf
        "((tool_name session_bridge) (executable %S) (executable_sha256 %s) (operations \
         (read status reference validate)) (read_roots (%S)) (environment \
         (\"PATH=/bin:/usr/bin\")) (private_paths (%S)))"
        helper
        sha
        workspace
        token_file
    in
    let text policies =
      String.substr_replace_first
        (config_text ~workspace ~prompt ~http:"((enabled false))")
        ~pattern:"(shutdown_grace_ms 30000)"
        ~with_:("(session_helpers (" ^ policies ^ "))")
    in
    let configured = validated_exn env source_file (text policy) in
    let captured = List.hd_exn configured.server.session_helpers in
    [%test_eq: string list]
      [ "read"; "status"; "reference"; "validate" ]
      captured.operations;
    assert (String.equal captured.executable helper);
    let revised =
      { configured with server = { configured.server with session_helpers = [] } }
    in
    assert
      (Agent_server.Config_diff.between ~previous:configured ~current:revised)
        .server_changed;
    let reject label value =
      match validate env source_file (text value) with
      | Error _ -> print_endline (label ^ ": rejected")
      | Ok _ -> failwith (label ^ " unexpectedly admitted")
    in
    reject "duplicate tool grants" (policy ^ " " ^ policy);
    List.iter
      [ "unknown operation", "(read status reference validate)", "(read approve)"
      ; "invalid digest", sha, "no-digest"
      ; ( "broader read root"
        , sprintf "(read_roots (%S))" workspace
        , sprintf "(read_roots (%S))" temporary )
      ; ( "implicit platform credential"
        , sprintf "(private_paths (%S))" token_file
        , "(private_paths (/etc/ochat/private-token))" )
      ; ( "duplicate environment key"
        , "(environment (\"PATH=/bin:/usr/bin\"))"
        , "(environment (\"PATH=/bin\" \"PATH=/usr/bin\"))" )
      ; ( "zero frame budget"
        , "(tool_name session_bridge)"
        , "(tool_name session_bridge) (max_request_bytes 0)" )
      ]
      ~f:(fun (label, pattern, with_) ->
        reject label (String.substr_replace_first policy ~pattern ~with_));
    let alias = Filename.concat temporary "public-link" in
    Eio.Path.symlink ~link_to:workspace Eio.Path.(Eio.Stdenv.fs env / alias);
    let linked =
      String.substr_replace_first
        policy
        ~pattern:(sprintf "(read_roots (%S))" workspace)
        ~with_:(sprintf "(read_roots (%S))" alias)
    in
    ignore (validated_exn env source_file (text linked) : Agent_server.Config.t);
    Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / alias);
    Eio.Path.symlink ~link_to:temporary Eio.Path.(Eio.Stdenv.fs env / alias);
    reject "symlink to private ancestor" linked;
    print_endline "explicit operations captured; helper policy changes require restart");
  [%expect
    {|
    duplicate tool grants: rejected
    unknown operation: rejected
    invalid digest: rejected
    broader read root: rejected
    implicit platform credential: rejected
    duplicate environment key: rejected
    zero frame budget: rejected
    symlink to private ancestor: rejected
    explicit operations captured; helper policy changes require restart
  |}]
;;

let%expect_test "configuration normalizes paths and preflights ChatMD" =
  with_fixture (fun env temporary workspace prompt token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      sprintf
        "((enabled true) (address \"127.0.0.1\") (port 8787) (require_auth true) \
         (static_tokens_file \"%s\"))"
        token_file
    in
    let config =
      validate env source_file (config_text ~workspace ~prompt ~http)
      |> function
      | Ok value -> value
      | Error diagnostics ->
        raise_s
          [%sexp
            "unexpected validation diagnostics"
          , (diagnostics : Agent_server.Config.Diagnostic.t list)]
    in
    print_s
      [%sexp
        { data_dir_resolved =
            (String.equal config.server.data_dir (Filename.concat temporary "data")
             : bool)
        ; prompt_count = (List.length config.prompts : int)
        ; workspace_count = (List.length config.workspaces : int)
        ; profile_count = (List.length config.permission_profiles : int)
        ; manifest_grant_count = (List.length config.manifest_grants : int)
        ; job_daemon_total = (config.server.job_limits.daemon_total : int)
        ; max_nested_depth = (config.server.job_limits.max_nested_depth : int)
        ; max_http_connections = (config.server.http.max_connections : int)
        ; http_idle_timeout_ms = (config.server.http.idle_connection_timeout_ms : int)
        ; response_artifact_ms =
            (config.server.event_retention.response_artifact_ms : int)
        }]);
  [%expect
    {|
    ((data_dir_resolved true) (prompt_count 1) (workspace_count 1)
     (profile_count 1) (manifest_grant_count 0) (job_daemon_total 16)
     (max_nested_depth 8) (max_http_connections 1024)
     (http_idle_timeout_ms 300000) (response_artifact_ms 3600000))
    |}]
;;

let%expect_test "response artifact retention must be positive" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      "((enabled false) (address \"127.0.0.1\") (port 8787) (require_auth true))"
    in
    let contents =
      config_text ~workspace ~prompt ~http
      |> String.substr_replace_first
           ~pattern:"(completed_stream_ms 3600000)"
           ~with_:"(completed_stream_ms 3600000) (response_artifact_ms 0)"
    in
    let code =
      match validate env source_file contents with
      | Error (diagnostic :: _) -> diagnostic.code
      | Error [] -> "empty"
      | Ok _ -> "accepted"
    in
    print_endline code);
  [%expect {| config.range |}]
;;

let%expect_test "HTTP connection limit must be positive" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      "((enabled false) (address \"127.0.0.1\") (port 8787) (require_auth true) \
       (max_connections 0))"
    in
    let code =
      match validate env source_file (config_text ~workspace ~prompt ~http) with
      | Error (diagnostic :: _) -> diagnostic.code
      | Error [] -> "empty"
      | Ok _ -> "accepted"
    in
    print_endline code);
  [%expect {| config.range |}]
;;

let%expect_test "HTTP idle connection timeout must be positive" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      "((enabled false) (address \"127.0.0.1\") (port 8787) (require_auth true) \
       (idle_connection_timeout_ms 0))"
    in
    let code =
      match validate env source_file (config_text ~workspace ~prompt ~http) with
      | Error (diagnostic :: _) -> diagnostic.code
      | Error [] -> "empty"
      | Ok _ -> "accepted"
    in
    print_endline code);
  [%expect {| config.range |}]
;;

let%expect_test "OAuth and trusted-proxy authenticators satisfy HTTP validation" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let validate_http http =
      validate env source_file (config_text ~workspace ~prompt ~http) |> Result.is_ok
    in
    let oauth =
      validate_http
        "((enabled true) (address \"127.0.0.1\") (port 8787) (require_auth true) \
         (oauth_validator company-oidc))"
    in
    let reverse_proxy =
      validate_http
        "((enabled true) (address \"127.0.0.1\") (port 8787) (require_auth true) \
         (reverse_proxy ((trusted_addresses (127.0.0.1)) (principal_header \
         x-agent-principal) (scopes_header x-agent-scopes))))"
    in
    print_s [%sexp { oauth : bool; reverse_proxy : bool }]);
  [%expect {| ((oauth true) (reverse_proxy true)) |}]
;;

let%expect_test "reverse-proxy identity is accepted only from a trusted direct peer" =
  let principal_id = Agent_protocol.Id.Principal.create () in
  let headers =
    [ "x-agent-principal", Agent_protocol.Id.Principal.to_string principal_id
    ; "x-agent-scopes", "session.transcript.read, session.message.send"
    ]
  in
  let identity =
    Agent_server.Authenticator.Request_identity.
      { client_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 443); headers }
  in
  let authenticate trusted_addresses =
    Agent_server.Authenticator.authenticate_reverse_proxy
      ~trusted_addresses
      ~principal_header:"x-agent-principal"
      ~scopes_header:"x-agent-scopes"
      identity
  in
  let trusted =
    authenticate [ "127.0.0.1" ] |> Result.ok |> Option.join |> Option.value_exn
  in
  let spoof_ignored =
    match authenticate [ "10.0.0.1" ] with
    | Ok None -> true
    | Ok (Some _) | Error _ -> false
  in
  print_s
    [%sexp
      { principal_matches =
          (Agent_protocol.Id.Principal.compare trusted.id principal_id = 0 : bool)
      ; authentication_kind = (trusted.authentication_kind : string)
      ; can_read =
          (Agent_protocol.Principal.has_scope trusted View_session_transcript : bool)
      ; can_write = (Agent_protocol.Principal.has_scope trusted Send_messages : bool)
      ; spoof_ignored : bool
      }];
  [%expect
    {|
    ((principal_matches true) (authentication_kind http.reverse_proxy)
     (can_read true) (can_write true) (spoof_ignored true))
    |}]
;;

let%expect_test "connection attachment reservations enforce the negotiated limit" =
  Eio_main.run (fun _env ->
    let principal =
      Agent_protocol.Principal.create
        ~id:(Agent_protocol.Id.Principal.create ())
        ~authentication_kind:"test"
        ~scopes:Agent_protocol.Scope.Set.empty
        ~attributes:[]
      |> Result.ok
      |> Option.value_exn
    in
    let context =
      Agent_server.Connection_context.create
        ~connection_id:"attachment-limit-test"
        ~principal
        ~transport:In_memory
        ~publish_notification:ignore
        ~max_attachments:1
    in
    let first =
      Agent_server.Connection_context.reserve_attachment context |> Result.is_ok
    in
    let second =
      Agent_server.Connection_context.reserve_attachment context |> Result.is_ok
    in
    Agent_server.Connection_context.release_attachment_reservation context;
    let after_release =
      Agent_server.Connection_context.reserve_attachment context |> Result.is_ok
    in
    Agent_server.Connection_context.release_attachment_reservation context;
    print_s [%sexp { first : bool; second : bool; after_release : bool }]);
  [%expect {| ((first true) (second false) (after_release true)) |}]
;;

let%expect_test "job capacity enforces dimensions and releases leases" =
  with_fixture (fun _env _temporary _workspace _prompt _token_file ->
    let principal = Agent_protocol.Id.Principal.create () in
    let limits =
      Agent_server.Config.Server.
        { daemon_total = 2
        ; per_principal = 1
        ; per_prompt = 2
        ; per_workspace = 2
        ; per_session = 1
        ; per_kind = 2
        ; max_nested_depth = 1
        }
    in
    let capacity = Agent_server.Job_capacity.create ~limits in
    let key ?(depth = 0) principal_id session_id =
      Agent_server.Job_capacity.Key.create
        ~principal_id
        ~prompt:"prompt"
        ~workspace_conflict_domain:"workspace"
        ~session_id
        ~kind:Agent_protocol.Job.Model_call
        ~nested_depth:depth
    in
    let first_session = Agent_protocol.Id.Session.create () in
    let second_session = Agent_protocol.Id.Session.create () in
    let first =
      Agent_server.Job_capacity.try_acquire capacity (key (Some principal) first_session)
      |> Result.ok
      |> Option.join
      |> Option.value_exn
    in
    let same_principal_blocked =
      Agent_server.Job_capacity.try_acquire capacity (key (Some principal) second_session)
      |> Result.equal (Option.equal phys_equal) (fun _ _ -> false) (Ok None)
    in
    let depth_rejected =
      match
        Agent_server.Job_capacity.try_acquire capacity (key ~depth:2 None second_session)
      with
      | Error error -> Agent_protocol.Error.equal_code error.code Resource_limit
      | Ok _ -> false
    in
    Agent_server.Job_capacity.release first;
    Agent_server.Job_capacity.release first;
    let reacquired =
      Agent_server.Job_capacity.try_acquire capacity (key (Some principal) second_session)
      |> Result.ok
      |> Option.join
      |> Option.is_some
    in
    print_s
      [%sexp { same_principal_blocked : bool; depth_rejected : bool; reacquired : bool }]);
  [%expect {| ((same_principal_blocked true) (depth_rejected true) (reacquired true)) |}]
;;

let manifest_grant_text ~manifest_sha256 ~source_sha256 ~principal =
  sprintf
    {|
(manifest_grants
 (((id coding-agent-exact)
   (prompt coding-agent)
   (workspaces (project))
   (manifest_sha256 %s)
   (source_sha256 %s)
   (principals (%s)))))
|}
    manifest_sha256
    source_sha256
    principal
;;

let%expect_test "exact operator manifest grants validate and compile" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      "((enabled false) (address \"127.0.0.1\") (port 8787) (require_auth true))"
    in
    let principal = Agent_protocol.Id.Principal.create () in
    let manifest_sha256 = String.make 64 'A' in
    let source_sha256 = String.make 64 'b' in
    let contents =
      config_text ~workspace ~prompt ~http
      ^ manifest_grant_text
          ~manifest_sha256
          ~source_sha256
          ~principal:(Agent_protocol.Id.Principal.to_string principal)
    in
    let config = validated_exn env source_file contents in
    let grant = List.hd_exn config.manifest_grants in
    let built =
      Agent_server.Catalog_builder.build config
      |> function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
    in
    let compiled = List.hd_exn built.manifest_grants in
    let authorized =
      Agent_server.Operator_manifest_grant.authorizes
        compiled
        ~prompt_definition_id:
          (Agent_server.Catalog_identity.prompt_definition "coding-agent")
        ~workspace_definition_id:
          (Agent_server.Catalog_identity.workspace_definition "project")
        ~principal_id:principal
        ~source_sha256
        ~manifest_sha256:(String.lowercase manifest_sha256)
    in
    print_s
      [%sexp
        { normalized_manifest = (grant.manifest_sha256 : string)
        ; compiled_count = (List.length built.manifest_grants : int)
        ; authorized : bool
        }]);
  [%expect
    {|
    ((normalized_manifest
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
     (compiled_count 1) (authorized true))
    |}]
;;

let%expect_test "operator manifest grants reject malformed hashes" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      "((enabled false) (address \"127.0.0.1\") (port 8787) (require_auth true))"
    in
    let contents =
      config_text ~workspace ~prompt ~http
      ^ manifest_grant_text
          ~manifest_sha256:"not-a-hash"
          ~source_sha256:(String.make 64 'b')
          ~principal:
            (Agent_protocol.Id.Principal.create ()
             |> Agent_protocol.Id.Principal.to_string)
    in
    let code =
      match validate env source_file contents with
      | Error (diagnostic :: _) -> diagnostic.code
      | Error [] -> "empty"
      | Ok _ -> "accepted"
    in
    print_endline code);
  [%expect {| config.invalid_sha256 |}]
;;

let%expect_test "operator manifest grants require prompt-allowed workspaces" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      "((enabled false) (address \"127.0.0.1\") (port 8787) (require_auth true))"
    in
    let grant =
      manifest_grant_text
        ~manifest_sha256:(String.make 64 'a')
        ~source_sha256:(String.make 64 'b')
        ~principal:
          (Agent_protocol.Id.Principal.create () |> Agent_protocol.Id.Principal.to_string)
      |> String.substr_replace_all
           ~pattern:"(workspaces (project))"
           ~with_:"(workspaces (missing))"
    in
    let code =
      match validate env source_file (config_text ~workspace ~prompt ~http ^ grant) with
      | Error (diagnostic :: _) -> diagnostic.code
      | Error [] -> "empty"
      | Ok _ -> "accepted"
    in
    print_endline code);
  [%expect {| config.missing_reference |}]
;;

let%expect_test "named reviewer fallbacks resolve through daemon injection" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      "((enabled false) (address \"127.0.0.1\") (port 8787) (require_auth true))"
    in
    let contents =
      config_text ~workspace ~prompt ~http
      |> String.substr_replace_all
           ~pattern:"(approval_fallback deny)"
           ~with_:"(approval_fallback (external_reviewer security-gate))"
    in
    let config = validated_exn env source_file contents in
    let reviewer =
      Agent_session.Permission_reviewer.create
        ~id:"security-gate"
        ~kind:External
        ~revision:"test-v1"
        ~review:(fun _ -> Ok Allow)
      |> function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
    in
    let build ?reviewer_resolver () =
      Agent_server.Catalog_builder.build ?reviewer_resolver config
      |> function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
    in
    let invocation =
      Agent_session.Permission_policy.
        { tool_name = "write_file"
        ; identity_digest = "identity"
        ; invocation_display = "write_file(<redacted>)"
        ; effects = [ "filesystem.write" ]
        }
    in
    let policy built =
      List.hd_exn built.Agent_server.Catalog_builder.permission_profiles
    in
    let injected =
      build
        ~reviewer_resolver:(fun kind id ->
          if
            Agent_session.Permission_reviewer.equal_kind kind External
            && String.equal id "security-gate"
          then Some reviewer
          else None)
        ()
      |> policy
      |> fun policy ->
      Agent_session.Permission_policy.equal_decision
        (Agent_session.Permission_policy.decide
           policy
           ~responder_available:false
           invocation)
        Request_review
      &&
      match Agent_session.Permission_policy.review policy invocation with
      | Ok Allow -> true
      | Ok (Deny _) | Error _ -> false
    in
    let missing_fails_closed =
      build ()
      |> policy
      |> fun policy ->
      Agent_session.Permission_policy.equal_decision
        (Agent_session.Permission_policy.decide
           policy
           ~responder_available:false
           invocation)
        Request_review
      && Result.is_error (Agent_session.Permission_policy.review policy invocation)
    in
    print_s [%sexp { injected : bool; missing_fails_closed : bool }]);
  [%expect {| ((injected true) (missing_fails_closed true)) |}]
;;

let%expect_test "unsafe remote HTTP listener is rejected" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      "((enabled true) (address \"0.0.0.0\") (port 8787) (require_auth false))"
    in
    let code =
      match validate env source_file (config_text ~workspace ~prompt ~http) with
      | Ok _ -> "accepted"
      | Error (diagnostic :: _) -> diagnostic.code
      | Error [] -> "empty"
    in
    print_endline code);
  [%expect {| config.unsafe_listener |}]
;;

let%expect_test "catalog construction assigns stable opaque IDs and allowed pairs" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    let source_file = Filename.concat temporary "server.sexp" in
    let http =
      "((enabled false) (address \"127.0.0.1\") (port 8787) (require_auth true))"
    in
    let config =
      validate env source_file (config_text ~workspace ~prompt ~http)
      |> function
      | Ok value -> value
      | Error diagnostics ->
        raise_s [%sexp (diagnostics : Agent_server.Config.Diagnostic.t list)]
    in
    let build () =
      Agent_server.Catalog_builder.build config
      |> function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
    in
    let first = build () in
    let second = build () in
    let first_prompt = List.hd_exn first.prompts in
    let second_prompt = List.hd_exn second.prompts in
    let workspace =
      Agent_session.Workspace_catalog.find_by_name first.workspaces "project"
      |> Option.value_exn
    in
    print_s
      [%sexp
        { stable_prompt_id =
            (Agent_protocol.Id.Prompt_definition.compare first_prompt.id second_prompt.id
             = 0
             : bool)
        ; opaque_prompt_prefix =
            (String.is_prefix
               (Agent_protocol.Id.Prompt_definition.to_string first_prompt.id)
               ~prefix:"prd_"
             : bool)
        ; allowed =
            (Agent_session.Prompt_definition.allows_workspace first_prompt workspace.id
             : bool)
        }]);
  [%expect
    {|
    ((stable_prompt_id true) (opaque_prompt_prefix true) (allowed true))
    |}]
;;

let%test_unit "pinned catalog rebuild and restore reject altered materialized imports" =
  with_fixture (fun env temporary workspace prompt _ ->
    let file name = Eio.Path.(Eio.Stdenv.fs env / temporary / name) in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      (file "agent.chatmd")
      "<import src=\"shared.chatmd\"/>";
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      (file "shared.chatmd")
      "<developer>original</developer>";
    let config =
      validated_exn
        env
        (Filename.concat temporary "config.sexp")
        (config_text ~workspace ~prompt ~http:"((enabled false))")
    in
    let store_ok = function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
    in
    let built = Agent_server.Catalog_builder.build config |> store_ok in
    let definition = List.hd_exn built.prompts in
    let artifact_store =
      Agent_store.Prompt_artifact_store.create
        ~env
        ~root:(Filename.concat temporary "artifacts")
      |> store_ok
    in
    let build () =
      Agent_session.Prompt_revision_builder.build
        ~env
        ~artifact_store
        ~transaction_id:(Agent_protocol.Id.Transaction.create ())
        ~created_at:(Agent_protocol.Timestamp.now ())
        definition
    in
    let revision =
      match build () with
      | Ok value -> value
      | Error errors ->
        raise_s [%sexp (errors : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
    in
    let tree = Agent_session.Prompt_revision.materialized_tree revision in
    Eio.Path.unlink Eio.Path.(tree / "shared.chatmd");
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(tree / "shared.chatmd")
      "<developer>altered</developer>";
    assert (Result.is_error (build ()));
    assert (
      Result.is_error
        (Agent_session.Prompt_revision_builder.restore
           ~artifact_store
           definition
           (Agent_session.Prompt_revision.id revision))))
;;

let%test_unit "captured source reads never fall back to changed files or external paths" =
  with_fixture (fun env temporary _ _ _ ->
    let root = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    let loader =
      Source_loader.captured_filesystem
        ~root
        ~sources:[ "root.chatmd", "root"; "nested/shared.chatmd", "captured" ]
    in
    let ok = Result.ok_or_failwith in
    let base = Source_loader.root loader ~file:"root.chatmd" |> ok in
    let source =
      Source_loader.resolve loader ~base ~reference:"nested/shared.chatmd" |> ok
    in
    assert (String.equal (Source_loader.read loader source |> ok) "captured");
    let missing = Source_loader.resolve loader ~base ~reference:"agent.chatmd" |> ok in
    assert (Result.is_error (Source_loader.read loader missing));
    List.iter
      [ "../agent.chatmd"; Filename.concat temporary "agent.chatmd" ]
      ~f:(fun reference ->
        assert (Result.is_error (Source_loader.resolve loader ~base ~reference));
        assert (Result.is_error (Source_loader.root loader ~file:reference))))
;;

let%test_unit "configuration polling publishes edits without an explicit reload" =
  with_fixture (fun env temporary workspace prompt _ ->
    Eio.Switch.run (fun sw ->
      let path = Filename.concat temporary "poll.sexp" in
      let contents = config_text ~workspace ~prompt ~http:"((enabled false))" in
      let file = Eio.Path.(Eio.Stdenv.fs env / path) in
      Eio.Path.save ~create:(`Exclusive 0o600) file contents;
      let initial = validated_exn env path contents in
      let commits = ref 0 in
      let watcher =
        Agent_server.Config_watcher.create
          ~sw
          ~env
          ~path
          ~initial
          ~hooks:
            { prepare =
                (fun _ diff _ ~checkpoint:_ ->
                  Ok (Agent_server.Config_watcher.Prepared_result.Reloaded diff))
            ; publish = (fun _ _ _ -> Ok ())
            ; commit = (fun _ _ _ -> incr commits)
            ; discard_prepared = (fun () -> Rejected)
            ; audit = (fun _ _ -> ())
            ; retire = (fun () -> ())
            }
      in
      Fun.protect
        ~finally:(fun () -> Agent_server.Config_watcher.close watcher)
        (fun () ->
           let clock = Eio.Stdenv.clock env in
           Agent_server.Config_watcher.run ~sw ~clock ~every:0.01 watcher;
           Eio.Time.sleep clock 0.05;
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             file
             (String.substr_replace_all
                contents
                ~pattern:"Coding agent"
                ~with_:"Auto reloaded");
           Eio.Time.with_timeout_exn clock 5. (fun () ->
             let rec await () =
               if !commits = 0
               then (
                 Eio.Time.sleep clock 0.01;
                 await ())
             in
             await ());
           let current = Agent_server.Config_watcher.current watcher in
           assert (
             Option.equal
               String.equal
               (List.hd_exn current.prompts).description
               (Some "Auto reloaded")))))
;;

let%expect_test "static bearer authentication accepts only the configured token" =
  with_fixture (fun env _temporary _workspace _prompt token_file ->
    let authenticator =
      Agent_server.Authenticator.load_static_file ~env ~path:token_file
      |> function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
    in
    let accepted =
      Agent_server.Authenticator.authenticate_bearer
        authenticator
        ~now:(Agent_protocol.Timestamp.now ())
        ~token:"test-token"
      |> Result.is_ok
    in
    let rejected =
      Agent_server.Authenticator.authenticate_bearer
        authenticator
        ~now:(Agent_protocol.Timestamp.now ())
        ~token:"wrong-token"
      |> Result.is_error
    in
    print_s [%sexp { accepted : bool; rejected : bool }]);
  [%expect {| ((accepted true) (rejected true)) |}]
;;

let%expect_test "daemon maintenance prunes expired idempotency and temporary blobs" =
  with_fixture (fun env temporary _workspace _prompt _token_file ->
    Eio.Switch.run (fun sw ->
      let store_ok = function
        | Ok value -> value
        | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
      in
      let protocol_ok = function
        | Ok value -> value
        | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
      in
      let root = Filename.concat temporary "maintenance" in
      let temporary_blobs = Filename.concat root "temporary-blobs" in
      let durable_blobs = Filename.concat root "durable-blobs" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
      let session_store =
        Agent_store.Session_store.create
          ~env
          ~sw
          ~root:(Filename.concat root "sessions-store")
          ~server_id:(Agent_protocol.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:"maintenance-test"
        |> store_ok
      in
      let idempotency_store =
        Agent_store.Idempotency_store.open_or_create
          ~env
          ~path:(Filename.concat root "idempotency.sexp")
        |> store_ok
      in
      let blob_store =
        Agent_store.Blob_store.create
          ~env
          ~temporary_directory:temporary_blobs
          ~durable_directory:durable_blobs
          ~max_upload_bytes:1024L
        |> store_ok
      in
      let principal_id = Agent_protocol.Id.Principal.create () in
      let expired_at =
        Agent_protocol.Timestamp.of_string "2026-08-16T11:00:00Z" |> protocol_ok
      in
      let now =
        Agent_protocol.Timestamp.of_string "2026-08-16T12:00:00Z" |> protocol_ok
      in
      let idempotency_key =
        Agent_protocol.Idempotency_key.of_string "maintenance-expired" |> protocol_ok
      in
      Agent_store.Idempotency_store.record
        idempotency_store
        { key =
            { principal_id
            ; session_id = None
            ; method_name = "session.start"
            ; idempotency_key
            }
        ; request_digest = "digest"
        ; accepted_transaction_sequence = None
        ; outcome = Success (`Object [])
        ; created_at = expired_at
        ; expires_at = Some expired_at
        ; retention = Standard
        }
      |> store_ok
      |> ignore;
      let upload =
        Agent_store.Blob_store.begin_upload
          blob_store
          ~sw
          ~id:(Agent_protocol.Id.Blob.create ())
          ~creating_principal:principal_id
          ~target_session:None
          ~kind:Agent_protocol.Blob.File
          ~media_type:"text/plain"
          ~display_name:None
          ~allowed_use:"test"
          ~created_at:expired_at
          ~expires_at:(Some expired_at)
        |> store_ok
      in
      Agent_store.Blob_store.write_string upload "expired" |> store_ok;
      Agent_store.Blob_store.finish upload ~expected_digest:None |> store_ok |> ignore;
      let stats =
        Agent_server.Maintenance.run_once
          ~collection_policy:Load_retained
          ~registry:None
          ~env
          ~idempotency_store
          ~blob_store
          ~session_store
          ~protected_response_sessions:[]
          ~response_retention:(Time_ns.Span.of_hr 1.)
          ~now
        |> store_ok
      in
      print_s [%sexp (stats : Agent_server.Maintenance.stats)]));
  [%expect
    {|
    ((expired_idempotency_records 1) (expired_temporary_blobs 1)
     (expired_response_artifacts 0) (discarded_job_results 0)
     (retired_job_preparations 0) (deferred_result_collections 0))
    |}]
;;

let%expect_test "daemon reload atomically replaces catalogs" =
  with_fixture (fun env temporary workspace prompt _token_file ->
    Eio.Switch.run (fun sw ->
      let module Revision = Agent_session.Prompt_revision in
      let module Catalog = Agent_session.Prompt_catalog in
      let document = Eio.Path.(Eio.Stdenv.fs env / temporary / "guide.md") in
      Eio.Path.save ~create:(`Exclusive 0o600) document "first captured guide";
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / prompt)
        {|<developer><doc src="guide.md" local capture /></developer>|};
      let revision daemon =
        let entry =
          Catalog.find_by_name (Agent_server.Daemon.prompts daemon) "coding-agent"
          |> Option.value_exn
        in
        match entry.availability with
        | Ready revision -> revision
        | Unavailable diagnostics ->
          raise_s
            [%sexp
              (diagnostics : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
        | Disabled -> failwith "unexpected disabled fixture"
      in
      let guide revision =
        match Revision.elements revision with
        | [ Prompt.Chat_markdown.Developer
              { content =
                  Some (Items [ Basic { text = Some text; document_url = None; _ } ])
              ; _
              }
          ] -> text
        | _ -> failwith "expected captured literal document"
      in
      let source_file = Filename.concat temporary "reload-server.sexp" in
      let http =
        "((enabled false) (address \"127.0.0.1\") (port 8787) (require_auth true))"
      in
      let initial_text = config_text ~workspace ~prompt ~http in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / source_file)
        initial_text;
      let initial = validated_exn env source_file initial_text in
      let daemon =
        Agent_server.Daemon.start
          ~sw
          ~env
          ~config:initial
          ~tool_dir:temporary
          ~home:temporary
          ~process_start_identity:(Some "reload-test")
          ()
        |> function
        | Ok value -> value
        | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
      in
      let initial_revision = revision daemon in
      [%test_eq: string] "first captured guide" (guide initial_revision);
      Eio.Path.save ~create:(`Or_truncate 0o600) document "updated captured guide";
      let updated_text =
        initial_text
        |> String.substr_replace_all
             ~pattern:"(access shared_write)"
             ~with_:"(access read_only)"
        |> String.substr_replace_all
             ~pattern:"(description \"Coding agent\")"
             ~with_:"(description \"Reloaded agent\")"
        |> String.substr_replace_all
             ~pattern:"(tool_default ask)"
             ~with_:"(tool_default allow)"
      in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / source_file)
        updated_text;
      let diff =
        Agent_server.Daemon.reload_config daemon
        |> function
        | Ok value -> value
        | Error diagnostics ->
          raise_s [%sexp (diagnostics : Agent_server.Config.Diagnostic.t list)]
      in
      let updated_revision = revision daemon in
      Eio.Path.unlink document;
      [%test_eq: string] "first captured guide" (guide initial_revision);
      [%test_eq: string] "updated captured guide" (guide updated_revision);
      assert (
        not
          (Agent_protocol.Id.Prompt_revision.equal
             (Revision.id initial_revision)
             (Revision.id updated_revision)));
      print_endline
        "startup and reload capture independently; retained revisions survive deletion";
      let workspace =
        Agent_session.Workspace_catalog.find_by_name
          (Agent_server.Daemon.workspaces daemon)
          "project"
        |> Option.value_exn
      in
      let prompt =
        Agent_session.Prompt_catalog.find_by_name
          (Agent_server.Daemon.prompts daemon)
          "coding-agent"
        |> Option.value_exn
      in
      print_s
        [%sexp
          { workspace_changed = (diff.workspaces.changed : string list)
          ; prompt_changed = (diff.prompts.changed : string list)
          ; profile_changed = (diff.permission_profiles.changed : string list)
          ; access = (workspace.access : Agent_session.Workspace_definition.access)
          ; description = (prompt.definition.description : string option)
          }];
      Agent_server.Daemon.shutdown daemon
      |> function
      | Ok () -> ()
      | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]));
  [%expect
    {|
    startup and reload capture independently; retained revisions survive deletion
    ((workspace_changed (project)) (prompt_changed (coding-agent))
     (profile_changed (interactive)) (access Read_only)
     (description ("Reloaded agent"))) |}]
;;

let%expect_test "proxy actor policy cannot replace authenticated authority" =
  with_fixture (fun env temporary workspace prompt _ ->
    Eio.Switch.run (fun sw ->
      let module P = Agent_protocol in
      let source_file = Filename.concat temporary "proxy-server.sexp" in
      let http =
        "((enabled true) (address \"127.0.0.1\") (port 8787) (require_auth true) \
         (reverse_proxy ((trusted_addresses (127.0.0.1)) (principal_header \
         x-agent-principal) (scopes_header x-agent-scopes))))"
      in
      let config = validated_exn env source_file (config_text ~workspace ~prompt ~http) in
      let transform = ref (fun (principal : P.Principal.t) -> principal) in
      let raises = ref false in
      let daemon =
        Agent_server.Daemon.start
          ~options:
            { Agent_server.Daemon.default_options with
              proxy_actor_policy =
                Some
                  (fun ~now:_ _ ~principal ->
                    if !raises then raise Exit;
                    Ok
                      (Operator_authorization.guarded
                         ~principal:(!transform principal)
                         ~is_current:(fun () -> true)))
            }
          ~sw
          ~env
          ~config
          ~tool_dir:temporary
          ~home:temporary
          ~process_start_identity:None
          ()
        |> function
        | Ok daemon -> daemon
        | Error error -> raise_s [%sexp (error : P.Error.t)]
      in
      Exn.protect
        ~finally:(fun () ->
          match Agent_server.Daemon.shutdown daemon with
          | Ok () -> ()
          | Error error -> raise_s [%sexp (error : P.Error.t)])
        ~f:(fun () ->
          let identity =
            Agent_server.Authenticator.Request_identity.
              { client_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 443)
              ; headers =
                  [ "x-agent-principal", "pri_original_proxy"
                  ; "x-agent-scopes", "provider.view"
                  ]
              }
          in
          let authenticate () =
            Agent_server.Daemon.authenticate_http_actor daemon identity None
          in
          assert (Result.is_ok (authenticate ()));
          let mutations =
            [ (fun (p : P.Principal.t) -> { p with id = P.Id.Principal.create () })
            ; (fun p -> { p with authentication_kind = "local.trusted" })
            ; (fun p -> { p with scopes = Set.add p.scopes Provider_manage })
            ; (fun p -> { p with attributes = [ "injected", "authority" ] })
            ]
          in
          List.iter mutations ~f:(fun mutation ->
            transform := mutation;
            match authenticate () with
            | Error { code = Unauthenticated; _ } -> ()
            | _ -> failwith "proxy authority replacement admitted");
          raises := true;
          (match authenticate () with
           | exception Exit -> ()
           | _ -> failwith "policy exception hidden");
          print_endline
            "exact authority accepted; identity/kind/scopes/attributes replacements \
             refused; policy exception preserved")));
  [%expect
    {| exact authority accepted; identity/kind/scopes/attributes replacements refused; policy exception preserved |}]
;;

let%test_module "fallible configuration publication" =
  (module struct
    module W = Agent_server.Config_watcher
    module Selection = Agent_protocol.Prompt_dependency_selection

    let protocol_value = function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
    ;;

    let request () =
      Selection.Update_request.create
        ~definitions:[ Agent_server.Catalog_identity.prompt_definition "coding-agent" ]
        ~expected_generation:(Selection.Generation.of_int 3 |> protocol_value)
        ~idempotency_key:
          (Agent_protocol.Idempotency_key.of_string "dependency-update" |> protocol_value)
      |> protocol_value
    ;;

    let result request =
      let previous_generation = Selection.Update_request.expected_generation request in
      Selection.Update_result.create
        ~previous_generation
        ~generation:
          (Selection.Generation.of_int
             (Selection.Generation.to_int previous_generation + 1)
           |> protocol_value)
        ~targets:
          (List.map
             (Selection.Update_request.definitions request)
             ~f:(fun definition_id ->
               Selection.Update_result.Target.
                 { definition_id
                 ; outcome = Unavailable { reason = "fixture unavailable" }
                 }))
      |> protocol_value
    ;;

    let prepared operation diff =
      Ok
        (match operation with
         | W.Operation.Reload -> W.Prepared_result.Reloaded diff
         | Update request -> Dependencies_updated (result request))
    ;;

    let hooks : W.hooks =
      { prepare = (fun operation diff _ ~checkpoint:_ -> prepared operation diff)
      ; publish = (fun _ _ _ -> Ok ())
      ; commit = (fun _ _ _ -> ())
      ; discard_prepared = (fun () -> Rejected)
      ; audit = (fun _ _ -> ())
      ; retire = (fun () -> ())
      }
    ;;

    let with_file f =
      with_fixture (fun env temporary workspace prompt _ ->
        Eio.Switch.run (fun sw ->
          let path = Filename.concat temporary "fallible.sexp" in
          let contents = config_text ~workspace ~prompt ~http:"((enabled false))" in
          let initial = validated_exn env path contents in
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / path)
            (String.substr_replace_all
               contents
               ~pattern:"Coding agent"
               ~with_:"Next configuration");
          f env sw path initial))
    ;;

    let description watcher =
      (W.current watcher).prompts
      |> List.hd_exn
      |> fun (prompt : Agent_server.Config.Prompt.t) -> prompt.description
    ;;

    let assert_description watcher text =
      [%test_eq: string option] (Some text) (description watcher)
    ;;

    let assert_state expected actual =
      if not (W.State.equal expected actual)
      then raise_s [%sexp (expected : W.State.t), (actual : W.State.t)]
    ;;

    let%expect_test "publication acknowledgment precedes pointer installation" =
      with_file (fun env sw path initial ->
        let watcher = ref None in
        let current () = Option.value_exn !watcher in
        let record phase =
          let watcher = current () in
          let state, _ = W.status watcher in
          print_s
            [%sexp
              (phase : string), (state : W.State.t), (description watcher : string option)]
        in
        let watcher_value =
          W.create
            ~sw
            ~env
            ~path
            ~initial
            ~hooks:
              { hooks with
                prepare =
                  (fun operation diff _ ~checkpoint:_ ->
                    record "prepare";
                    prepared operation diff)
              ; publish =
                  (fun _ _ _ ->
                    record "durable acknowledgment";
                    Ok ())
              ; commit = (fun _ _ _ -> record "synchronous commit")
              ; audit = (fun _ _ -> record "audit")
              ; retire = (fun () -> record "retire")
              }
        in
        watcher := Some watcher_value;
        assert (Result.is_ok (W.reload watcher_value));
        record "installed";
        W.close_and_wait watcher_value);
      [%expect
        {|
        (prepare Busy ("Coding agent"))
        ("durable acknowledgment" Busy ("Coding agent"))
        ("synchronous commit" Busy ("Coding agent"))
        (audit Busy ("Next configuration"))
        (retire Busy ("Next configuration"))
        (installed Available ("Next configuration"))
        |}]
    ;;

    let%test_unit "typed rejection and uncertainty retain old pointers and govern retry" =
      List.iter [ W.Certainty.Rejected; Uncertain ] ~f:(fun certainty ->
        with_file (fun env sw path initial ->
          let publications = ref 0 in
          let diagnostic =
            Agent_server.Config.Diagnostic.
              { code = "test.publish"
              ; config_path = "$"
              ; source_file = path
              ; message = "publication denied"
              ; remediation = "inspect publication"
              }
          in
          let watcher =
            W.create
              ~sw
              ~env
              ~path
              ~initial
              ~hooks:
                { hooks with
                  publish =
                    (fun _ _ _ ->
                      incr publications;
                      Error
                        (match certainty with
                         | Rejected -> W.Failure.Rejected [ diagnostic ]
                         | Uncertain -> Uncertain [ diagnostic ]))
                }
          in
          assert (Result.is_error (W.reload watcher));
          assert_description watcher "Coding agent";
          let state, _ = W.status watcher in
          assert_state
            (match certainty with
             | Rejected -> Available
             | Uncertain -> Uncertain)
            state;
          assert (Result.is_error (W.reload watcher));
          [%test_eq: int]
            (match certainty with
             | Rejected -> 2
             | Uncertain -> 1)
            !publications;
          W.close_and_wait watcher))
    ;;

    type phase =
      | Prepare
      | Publish
      | Commit
      | Audit
      | Retire

    let%test_unit "hook exceptions preserve identity and distinguish postcommit failure" =
      List.iter [ Prepare; Publish; Commit; Audit; Retire ] ~f:(fun phase ->
        with_file (fun env sw path initial ->
          let sentinel = Failure "original reload callback" in
          let discards = ref 0 in
          let retired = ref 0 in
          let fail () = raise sentinel in
          let watcher =
            W.create
              ~sw
              ~env
              ~path
              ~initial
              ~hooks:
                { prepare =
                    (fun operation diff _ ~checkpoint:_ ->
                      match phase with
                      | Prepare -> fail ()
                      | _ -> prepared operation diff)
                ; publish =
                    (fun _ _ _ ->
                      match phase with
                      | Publish -> fail ()
                      | _ -> Ok ())
                ; commit =
                    (fun _ _ _ ->
                      match phase with
                      | Commit -> fail ()
                      | _ -> ())
                ; audit =
                    (fun _ _ ->
                      match phase with
                      | Audit -> fail ()
                      | _ -> ())
                ; retire =
                    (fun () ->
                      incr retired;
                      match phase with
                      | Retire -> fail ()
                      | _ -> ())
                ; discard_prepared =
                    (fun () ->
                      incr discards;
                      match phase with
                      | Publish -> Uncertain
                      | _ -> Rejected)
                }
          in
          (match W.reload watcher with
           | _ -> assert false
           | exception exn -> assert (phys_equal exn sentinel));
          let committed =
            match phase with
            | Audit | Retire -> true
            | Prepare | Publish | Commit -> false
          in
          assert_description
            watcher
            (if committed then "Next configuration" else "Coding agent");
          [%test_eq: int] (if committed then 1 else 0) !retired;
          [%test_eq: int] (if committed then 0 else 1) !discards;
          let state, _ = W.status watcher in
          assert_state
            (match phase with
             | Prepare | Audit | Retire -> Available
             | Publish | Commit -> Uncertain)
            state;
          W.close_and_wait watcher))
    ;;

    let%test_unit "Busy and close join borrowed reload without mutex-held hooks" =
      with_file (fun env sw path initial ->
        let entered, enter = Eio.Promise.create () in
        let leave, _ = Eio.Promise.create () in
        let cleaned = ref false in
        let calls = ref 0 in
        let watcher =
          W.create
            ~sw
            ~env
            ~path
            ~initial
            ~hooks:
              { hooks with
                prepare =
                  (fun operation diff _ ~checkpoint:_ ->
                    incr calls;
                    Eio.Promise.resolve enter ();
                    Fun.protect
                      ~finally:(fun () -> cleaned := true)
                      (fun () ->
                         Eio.Promise.await leave;
                         prepared operation diff))
              }
        in
        let reload = Eio.Fiber.fork_promise ~sw (fun () -> W.reload watcher) in
        Eio.Promise.await entered;
        assert_state Busy (fst (W.status watcher));
        assert (Result.is_error (W.reload watcher));
        assert (Result.is_error (W.check_admission watcher));
        assert_description watcher "Coding agent";
        [%test_eq: int] 1 !calls;
        W.close_and_wait watcher;
        assert !cleaned;
        (match Eio.Promise.await reload with
         | Error (Eio.Cancel.Cancelled _) -> ()
         | Error exn -> raise exn
         | Ok _ -> assert false);
        assert_state Closed (fst (W.status watcher));
        assert (Result.is_error (W.reload watcher)))
    ;;

    let%test_unit
        "caller cancellation preserves exact cause and complete publication correlations"
      =
      List.iter [ false; true ] ~f:(fun cancel_after_ack ->
        with_file (fun env sw path initial ->
          let context = ref None in
          let sentinel = Failure "original caller cancellation" in
          let cancel () = Eio.Cancel.cancel (Option.value_exn !context) sentinel in
          let publications = ref 0 in
          let commits = ref 0 in
          let discards = ref 0 in
          let retirements = ref 0 in
          let watcher =
            W.create
              ~sw
              ~env
              ~path
              ~initial
              ~hooks:
                { hooks with
                  prepare =
                    (fun operation diff _ ~checkpoint:_ ->
                      if not cancel_after_ack then cancel ();
                      prepared operation diff)
                ; publish =
                    (fun _ _ _ ->
                      incr publications;
                      cancel ();
                      Ok ())
                ; commit = (fun _ _ _ -> incr commits)
                ; discard_prepared =
                    (fun () ->
                      incr discards;
                      Rejected)
                ; retire = (fun () -> incr retirements)
                }
          in
          (match
             Eio.Cancel.sub (fun current ->
               context := Some current;
               W.reload watcher)
           with
           | _ -> assert false
           | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason sentinel));
          (* The worker may finish before its caller forwards cancellation. These
             are the complete legal outcomes, including acknowledged-but-uninstalled
             uncertainty and already-installed result-delivery cancellation. *)
          if cancel_after_ack then [%test_eq: int] 1 !publications;
          (match !publications, !commits with
           | 0, 0 ->
             assert_description watcher "Coding agent";
             assert_state Available (fst (W.status watcher));
             [%test_eq: int] !discards 1;
             [%test_eq: int] !retirements 0
           | 1, 0 ->
             assert_description watcher "Coding agent";
             assert_state Uncertain (fst (W.status watcher));
             [%test_eq: int] !discards 1;
             [%test_eq: int] !retirements 0
           | 1, 1 ->
             assert_description watcher "Next configuration";
             assert_state Available (fst (W.status watcher));
             [%test_eq: int] !discards 0;
             [%test_eq: int] !retirements 1
           | _ -> assert false);
          W.close_and_wait watcher))
    ;;

    let%test_unit "cancellation after worker resolution preserves installed state" =
      with_file (fun env sw path initial ->
        let caller = ref None in
        let sentinel = Failure "cancel resolved reload delivery" in
        let commits = ref 0 in
        let retirements = ref 0 in
        let discards = ref 0 in
        let watcher =
          W.create
            ~sw
            ~env
            ~path
            ~initial
            ~hooks:
              { hooks with
                commit = (fun _ _ _ -> incr commits)
              ; retire =
                  (fun () ->
                    incr retirements;
                    Eio.Fiber.fork ~sw (fun () ->
                      (* The fork runs this child first. Yield lets the worker
                         return and resolve its promise before cancellation;
                         this child then precedes the resumed caller. *)
                      Eio.Fiber.yield ();
                      Eio.Cancel.cancel (Option.value_exn !caller) sentinel))
              ; discard_prepared =
                  (fun () ->
                    incr discards;
                    Rejected)
              }
        in
        (match
           Eio.Cancel.sub (fun context ->
             caller := Some context;
             W.reload watcher)
         with
         | _ -> assert false
         | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason sentinel));
        [%test_eq: int] 1 !commits;
        [%test_eq: int] 1 !retirements;
        [%test_eq: int] 0 !discards;
        assert_description watcher "Next configuration";
        assert_state Available (fst (W.status watcher));
        let _, diagnostic = W.status watcher in
        assert (Option.is_some diagnostic);
        W.close_and_wait watcher)
    ;;

    let%expect_test "dependency update retains installed config and pending file edit" =
      with_file (fun env sw path initial ->
        let operation_names = ref [] in
        let watcher =
          W.create
            ~sw
            ~env
            ~path
            ~initial
            ~hooks:
              { hooks with
                prepare =
                  (fun operation diff config ~checkpoint ->
                    checkpoint ();
                    (match operation with
                     | Reload -> ()
                     | Update _ ->
                       assert (phys_equal config initial);
                       assert (Agent_server.Config_diff.is_empty diff));
                    prepared operation diff)
              ; audit =
                  (fun operation _ ->
                    operation_names
                    := (match operation with
                        | Reload -> "reload"
                        | Update _ -> "update")
                       :: !operation_names)
              }
        in
        let request = request () in
        let file = Eio.Path.(Eio.Stdenv.fs env / path) in
        let edited_contents = Eio.Path.load file in
        Eio.Path.unlink file;
        let updated = W.update_dependencies watcher request ~checkpoint:Eio.Fiber.check in
        (match updated with
         | Ok updated -> assert (Selection.Update_result.matches_request updated request)
         | Error failure -> raise_s [%sexp (failure : W.Failure.t)]);
        assert (phys_equal (W.current watcher) initial);
        print_s [%sexp (description watcher : string option)];
        Eio.Path.save ~create:(`Exclusive 0o600) file edited_contents;
        assert (Result.is_ok (W.reload watcher));
        print_s [%sexp (description watcher : string option)];
        print_s [%sexp (List.rev !operation_names : string list)];
        W.close_and_wait watcher);
      [%expect
        {|
        ("Coding agent")
        ("Next configuration")
        (update reload)
        |}]
    ;;

    let%test_unit "operation response affinity is checked before any writer" =
      List.iter [ false; true ] ~f:(fun update ->
        with_file (fun env sw path initial ->
          let publications = ref 0 in
          let discards = ref 0 in
          let watcher =
            W.create
              ~sw
              ~env
              ~path
              ~initial
              ~hooks:
                { hooks with
                  prepare =
                    (fun operation diff _ ~checkpoint:_ ->
                      Ok
                        (match operation with
                         | Reload -> Dependencies_updated (result (request ()))
                         | Update _ -> Reloaded diff))
                ; publish =
                    (fun _ _ _ ->
                      incr publications;
                      Ok ())
                ; discard_prepared =
                    (fun () ->
                      incr discards;
                      Rejected)
                }
          in
          let denied =
            if update
            then
              W.update_dependencies watcher (request ()) ~checkpoint:Eio.Fiber.check
              |> Result.map ~f:(fun _ -> ())
            else W.reload watcher |> Result.map ~f:(fun _ -> ())
          in
          (match denied with
           | Error (Rejected [ diagnostic ]) ->
             [%test_eq: string] "config.operation_result_mismatch" diagnostic.code
           | Ok () | Error (Rejected _ | Uncertain _) -> assert false);
          [%test_eq: int] 0 !publications;
          [%test_eq: int] 1 !discards;
          assert_state Available (fst (W.status watcher));
          assert (phys_equal (W.current watcher) initial);
          W.close_and_wait watcher));
      with_file (fun env sw path initial ->
        let original_request = request () in
        let foreign_request =
          Selection.Update_request.create
            ~definitions:(Selection.Update_request.definitions original_request)
            ~expected_generation:(Selection.Generation.of_int 9 |> protocol_value)
            ~idempotency_key:(Selection.Update_request.idempotency_key original_request)
          |> protocol_value
        in
        let publications = ref 0 in
        let watcher =
          W.create
            ~sw
            ~env
            ~path
            ~initial
            ~hooks:
              { hooks with
                prepare =
                  (fun _ _ _ ~checkpoint:_ ->
                    Ok (Dependencies_updated (result foreign_request)))
              ; publish =
                  (fun _ _ _ ->
                    incr publications;
                    Ok ())
              }
        in
        assert (
          Result.is_error
            (W.update_dependencies watcher original_request ~checkpoint:Eio.Fiber.check));
        [%test_eq: int] 0 !publications;
        assert_state Available (fst (W.status watcher));
        W.close_and_wait watcher)
    ;;

    let%test_unit "borrowed update checkpoint preserves exceptions only before writer" =
      List.iter [ None; Some 1; Some 2; Some 3 ] ~f:(fun stop_at ->
        with_file (fun env sw path initial ->
          let sentinel = Failure "original actor checkpoint" in
          let polls = ref 0 in
          let publications = ref 0 in
          let commits = ref 0 in
          let discards = ref 0 in
          let checkpoint () =
            incr polls;
            (* Any arbitrary callback after writer entry is a contract violation. *)
            [%test_eq: int] 0 !publications;
            if Option.equal Int.equal stop_at (Some !polls) then raise sentinel
          in
          let watcher =
            W.create
              ~sw
              ~env
              ~path
              ~initial
              ~hooks:
                { hooks with
                  prepare =
                    (fun operation diff _ ~checkpoint:borrowed ->
                      assert (phys_equal borrowed checkpoint);
                      borrowed ();
                      prepared operation diff)
                ; publish =
                    (fun _ _ _ ->
                      incr publications;
                      Ok ())
                ; commit = (fun _ _ _ -> incr commits)
                ; discard_prepared =
                    (fun () ->
                      incr discards;
                      Rejected)
                }
          in
          (match stop_at with
           | None ->
             assert (Result.is_ok (W.update_dependencies watcher (request ()) ~checkpoint));
             [%test_eq: int] 3 !polls;
             [%test_eq: int] 1 !publications;
             [%test_eq: int] 1 !commits;
             [%test_eq: int] 0 !discards
           | Some stop ->
             (match W.update_dependencies watcher (request ()) ~checkpoint with
              | _ -> assert false
              | exception exn -> assert (phys_equal sentinel exn));
             [%test_eq: int] stop !polls;
             [%test_eq: int] 0 !publications;
             [%test_eq: int] 0 !commits;
             [%test_eq: int] 1 !discards);
          assert_state Available (fst (W.status watcher));
          W.close_and_wait watcher))
    ;;

    let%test_unit "update audit exception preserves original failure after commit" =
      with_file (fun env sw path initial ->
        let sentinel = Failure "original update audit" in
        let commits = ref 0 in
        let retirements = ref 0 in
        let discards = ref 0 in
        let watcher =
          W.create
            ~sw
            ~env
            ~path
            ~initial
            ~hooks:
              { hooks with
                commit = (fun _ _ _ -> incr commits)
              ; audit = (fun _ _ -> raise sentinel)
              ; retire = (fun () -> incr retirements)
              ; discard_prepared =
                  (fun () ->
                    incr discards;
                    Rejected)
              }
        in
        (match W.update_dependencies watcher (request ()) ~checkpoint:Eio.Fiber.check with
         | _ -> assert false
         | exception exn -> assert (phys_equal sentinel exn));
        [%test_eq: int] 1 !commits;
        [%test_eq: int] 1 !retirements;
        [%test_eq: int] 0 !discards;
        assert (phys_equal (W.current watcher) initial);
        assert_state Available (fst (W.status watcher));
        assert (Option.is_some (snd (W.status watcher)));
        W.close_and_wait watcher)
    ;;

    let%test_unit "update and reload share Busy and uncertain admission" =
      with_file (fun env sw path initial ->
        let entered, enter = Eio.Promise.create () in
        let leave, release = Eio.Promise.create () in
        let diagnostic =
          Agent_server.Config.Diagnostic.
            { code = "test.uncertain"
            ; config_path = "$"
            ; source_file = path
            ; message = "writer acknowledgment unavailable"
            ; remediation = "reconcile history"
            }
        in
        let watcher =
          W.create
            ~sw
            ~env
            ~path
            ~initial
            ~hooks:
              { hooks with
                prepare =
                  (fun operation diff _ ~checkpoint:_ ->
                    Eio.Promise.resolve enter ();
                    Eio.Promise.await leave;
                    prepared operation diff)
              ; publish = (fun _ _ _ -> Error (Uncertain [ diagnostic ]))
              }
        in
        let update =
          Eio.Fiber.fork_promise ~sw (fun () ->
            W.update_dependencies watcher (request ()) ~checkpoint:Eio.Fiber.check)
        in
        Eio.Promise.await entered;
        assert_state Busy (fst (W.status watcher));
        assert (Result.is_error (W.reload watcher));
        assert (
          Result.is_error
            (W.update_dependencies watcher (request ()) ~checkpoint:Eio.Fiber.check));
        Eio.Promise.resolve release ();
        (match Eio.Promise.await update with
         | Ok (Error (Uncertain _)) -> ()
         | Ok _ -> assert false
         | Error exn -> raise exn);
        assert_state Uncertain (fst (W.status watcher));
        assert (phys_equal (W.current watcher) initial);
        assert (Result.is_error (W.reload watcher));
        assert (
          Result.is_error
            (W.update_dependencies watcher (request ()) ~checkpoint:Eio.Fiber.check));
        W.close_and_wait watcher)
    ;;

    let%test_unit "foreign domains cannot enter hooks or mutate watcher state" =
      with_file (fun env sw path initial ->
        let watcher = W.create ~sw ~env ~path ~initial ~hooks in
        Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
          List.iter
            [ (fun () -> ignore (W.reload watcher))
            ; (fun () -> W.close watcher)
            ; (fun () -> W.close_and_wait watcher)
            ; (fun () -> ignore (W.current watcher))
            ; (fun () -> ignore (W.status watcher))
            ; (fun () -> ignore (W.check_admission watcher))
            ]
            ~f:(fun operation ->
              match operation () with
              | () -> assert false
              | exception Invalid_argument _ -> ()));
        assert_state Available (fst (W.status watcher));
        W.close_and_wait watcher)
    ;;
  end)
;;
