open Core
module Config_fixture = Support.Config_fixture
module Daemon_host = Support.Daemon_host
module Http_driver = Support.Http_driver
module Temporary_environment = Support.Temporary_environment

let require condition message =
  if not condition
  then raise_s [%sexp "auth/security assertion failed", (message : string)]
;;

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "protocol failure", (error : Agent_protocol.Error.t)]
;;

let http_ok = function
  | Ok value -> value
  | Error message -> raise_s [%sexp "HTTP failure", (message : string)]
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    port)
;;

let fixture env environment name =
  Config_fixture.create environment ~name ~http_port:(reserve_port env)
;;

let configure fixture fields =
  let contents = Config_fixture.configuration fixture () in
  let contents =
    String.substr_replace_all
      contents
      ~pattern:"(require_auth true)"
      ~with_:("(require_auth true) " ^ fields)
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path
       (Config_fixture.environment fixture)
       (Config_fixture.config_path fixture))
    contents
;;

let with_client ~sw env fixture token f =
  let client =
    Http_driver.create ~sw ~env ~port:(Config_fixture.http_port fixture) ~token |> http_ok
  in
  Exn.protect ~f:(fun () -> f client) ~finally:(fun () -> Http_driver.shutdown client)
;;

let initialize_body =
  {|{"jsonrpc":"2.0","id":1,"method":"protocol.initialize","params":{"implementation":{"name":"auth-security-e2e","version":"dev"},"protocol_min":{"major":1,"minor":0},"protocol_max":{"major":1,"minor":0},"features":[],"event_encodings":["json"],"max_inbound_event_bytes":16777216}}|}
;;

let bearer token = [ "authorization", "Bearer " ^ token ]
let secret = "oauth-exception-secret-must-not-escape"

let require_redacted response =
  let wire =
    response.Http_driver.body
    ^ Sexp.to_string
        ([%sexp_of: (string * string) list] (Piaf.Headers.to_list response.headers))
  in
  require (not (String.is_substring wire ~substring:secret)) "callback secret leaked"
;;

let denied response =
  require (response.Http_driver.status = 401) "authentication did not return HTTP 401";
  require
    (Option.equal
       String.equal
       (Piaf.Headers.get response.headers "www-authenticate")
       (Some "Bearer"))
    "missing Bearer challenge";
  let error =
    Jsonaf.of_string response.body |> Agent_protocol.Error.of_json |> protocol_ok
  in
  require (Agent_protocol.Error.equal_code error.code Unauthenticated) "wrong auth error";
  require
    (String.equal error.message "authentication is required")
    "non-redacted HTTP error";
  require
    (Option.is_none (Piaf.Headers.get response.headers "ochat-connection-id"))
    "rejected authentication allocated a connection";
  require_redacted response
;;

let raw_initialize ~sw env fixture headers =
  with_client ~sw env fixture None (fun client ->
    Http_driver.rpc_raw client ~headers initialize_body |> http_ok)
;;

let accepted response kind =
  require (response.Http_driver.status = 200) "valid identity was rejected";
  let initialized =
    Jsonaf.of_string response.body
    |> Jsonaf.member_exn "result"
    |> Agent_protocol.Initialize.Response.of_json
    |> protocol_ok
  in
  require (String.equal initialized.principal.authentication_kind kind) "wrong auth kind";
  require
    (Option.is_some (Piaf.Headers.get response.headers "ochat-connection-id"))
    "accepted identity did not allocate a connection";
  initialized.principal
;;

let auth_error code message =
  Agent_protocol.Error.create
    code
    ~message
    ~retryable:false
    ~data:(`Object [ "secret", `String secret ])
    ()
;;

let oauth_principal () =
  Agent_protocol.Principal.create
    ~id:(Agent_protocol.Id.Principal.create ())
    ~authentication_kind:"http.oauth"
    ~scopes:Agent_protocol.Scope.Set.empty
    ~attributes:[]
  |> protocol_ok
;;

let validate_oauth principal ~now ~token =
  match token with
  | "oauth-valid" -> Ok principal
  | "oauth-expired" ->
    let expiry =
      Agent_protocol.Timestamp.of_string "2000-01-01T00:00:00Z" |> protocol_ok
    in
    require (Agent_protocol.Timestamp.compare now expiry >= 0) "test token is not expired";
    Error (auth_error Unauthenticated ("expired " ^ secret))
  | "oauth-throwing" -> failwith secret
  | "oauth-cancelled" -> raise (Eio.Cancel.Cancelled (Failure secret))
  | "oauth-unavailable" -> Error (auth_error Internal_error ("unavailable " ^ secret))
  | _ -> Error (auth_error Unauthenticated ("invalid " ^ secret))
;;

let oauth_options principal =
  { Agent_server.Daemon.default_options with
    oauth_resolver =
      Some
        (fun id ->
          require (String.equal id "e2e-oauth") "unexpected OAuth resolver ID";
          Some (validate_oauth principal))
  }
;;

let with_oauth env environment name f =
  let fixture = fixture env environment name in
  configure fixture "(oauth_validator e2e-oauth)";
  Temporary_environment.register_secret environment secret;
  let principal = oauth_principal () in
  Daemon_host.with_ env fixture ~options:(oauth_options principal) (fun sw daemon ->
    f sw fixture daemon principal)
;;

let test_oauth_valid env environment =
  with_oauth env environment "oauth-valid" (fun sw fixture _daemon expected ->
    let response = raw_initialize ~sw env fixture (bearer "oauth-valid") in
    let principal = accepted response "http.oauth" in
    require
      (Agent_protocol.Id.Principal.compare principal.id expected.id = 0)
      "wrong OAuth principal";
    require (Set.is_empty principal.scopes) "OAuth scopes were elevated";
    let response =
      raw_initialize ~sw env fixture (bearer (Config_fixture.admin_token fixture))
    in
    let (_ : Agent_protocol.Principal.t) = accepted response "http.static_bearer" in
    ())
;;

let test_oauth_denied token env environment =
  with_oauth env environment token (fun sw fixture _daemon _principal ->
    raw_initialize ~sw env fixture (bearer token) |> denied;
    let response = raw_initialize ~sw env fixture (bearer "oauth-valid") in
    let (_ : Agent_protocol.Principal.t) = accepted response "http.oauth" in
    ())
;;

let test_callback_exception env environment =
  with_oauth env environment "callback-unit" (fun _sw _fixture daemon _principal ->
    match Agent_server.Daemon.authenticate_http_bearer daemon (Some "oauth-throwing") with
    | Ok _ -> require false "throwing callback authenticated"
    | Error error ->
      require
        (Agent_protocol.Error.equal_code error.code Unauthenticated)
        "wrong callback error";
      require
        (String.equal error.message "bearer token is invalid")
        "callback error was not redacted";
      require
        (not
           (String.is_substring
              (Agent_protocol.Error.to_json error |> Jsonaf.to_string)
              ~substring:secret))
        "callback details leaked")
;;

let test_callback_cancellation env environment =
  with_oauth
    env
    environment
    "callback-cancellation"
    (fun _sw _fixture daemon _principal ->
       let is_cancelled =
         match
           Agent_server.Daemon.authenticate_http_bearer daemon (Some "oauth-cancelled")
         with
         | _ -> false
         | exception Eio.Cancel.Cancelled (Failure message) -> String.equal message secret
       in
       require is_cancelled "authentication swallowed or changed cancellation")
;;

let invalid_static_headers valid =
  [ []
  ; bearer "unknown"
  ; [ "authorization", "Basic secret" ]
  ; [ "authorization", "Bearer" ]
  ; [ "authorization", "Bearer " ]
  ; [ "authorization", "Bearer " ^ valid ^ " extra" ]
  ; [ "authorization", "Bearer\t" ^ valid ]
  ; [ "authorization", "Bearer " ^ valid; "Authorization", "Bearer " ^ valid ]
  ; [ "authorization", "Bearer " ^ valid; "authorization", "Basic secret" ]
  ; [ "authorization", "Bearer " ^ valid ^ ", Bearer " ^ valid ]
  ]
;;

let test_static_headers env environment =
  let fixture = fixture env environment "static-headers" in
  Daemon_host.with_
    env
    fixture
    ~options:Agent_server.Daemon.default_options
    (fun sw _daemon ->
       let valid = Config_fixture.admin_token fixture in
       List.iter (invalid_static_headers valid) ~f:(fun headers ->
         raw_initialize ~sw env fixture headers |> denied);
       let response =
         raw_initialize ~sw env fixture [ "Authorization", "bEaReR " ^ valid ]
       in
       let (_ : Agent_protocol.Principal.t) = accepted response "http.static_bearer" in
       ())
;;

let expire_public_token fixture =
  let path =
    Filename.concat (Filename.dirname (Config_fixture.config_path fixture)) "tokens.sexp"
  in
  let path = Temporary_environment.path (Config_fixture.environment fixture) path in
  let contents = Eio.Path.load path |> Sexp.of_string in
  let records =
    match contents with
    | Sexp.List [ admin; Sexp.List public ] ->
      let public =
        List.map public ~f:(function
          | Sexp.List [ Sexp.Atom "expires_at"; _ ] ->
            Sexp.List [ Sexp.Atom "expires_at"; Sexp.Atom "2000-01-01T00:00:00Z" ]
          | field -> field)
      in
      Sexp.List [ admin; Sexp.List public ]
    | _ -> failwith "unexpected static fixture shape"
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) path (Sexp.to_string records)
;;

let test_static_expired env environment =
  let fixture = fixture env environment "static-expired" in
  expire_public_token fixture;
  Daemon_host.with_
    env
    fixture
    ~options:Agent_server.Daemon.default_options
    (fun sw _daemon ->
       raw_initialize ~sw env fixture (bearer (Config_fixture.public_token fixture))
       |> denied;
       let response =
         raw_initialize ~sw env fixture (bearer (Config_fixture.admin_token fixture))
       in
       let (_ : Agent_protocol.Principal.t) = accepted response "http.static_bearer" in
       ())
;;

let principal_header = "x-e2e-principal"
let scopes_header = "x-e2e-scopes"

let proxy_headers principal =
  [ principal_header, principal; scopes_header, "prompt.list" ]
;;

let with_proxy env environment name trusted f =
  let fixture = fixture env environment name in
  configure
    fixture
    (sprintf
       "(reverse_proxy ((trusted_addresses (%s)) (principal_header %s) (scopes_header \
        %s)))"
       trusted
       principal_header
       scopes_header);
  Daemon_host.with_
    env
    fixture
    ~options:Agent_server.Daemon.default_options
    (fun sw daemon -> f sw fixture daemon)
;;

let test_proxy_trusted env environment =
  with_proxy env environment "proxy-trusted" "127.0.0.1" (fun sw fixture _daemon ->
    let id = Agent_protocol.Id.Principal.create () in
    let headers = proxy_headers (Agent_protocol.Id.Principal.to_string id) in
    List.iter
      [ headers; headers @ bearer "invalid" ]
      ~f:(fun headers ->
        let principal =
          raw_initialize ~sw env fixture headers
          |> fun r -> accepted r "http.reverse_proxy"
        in
        require
          (Agent_protocol.Id.Principal.compare id principal.id = 0)
          "proxy principal changed";
        require
          (Set.equal principal.scopes (Agent_protocol.Scope.Set.singleton List_prompts))
          "proxy scopes changed");
    raw_initialize ~sw env fixture [] |> denied;
    let response =
      raw_initialize ~sw env fixture (bearer (Config_fixture.admin_token fixture))
    in
    let (_ : Agent_protocol.Principal.t) = accepted response "http.static_bearer" in
    ())
;;

let proxy_errors principal =
  [ [ principal_header, principal ]
  ; [ scopes_header, "prompt.list" ]
  ; [ principal_header, ""; scopes_header, "prompt.list" ]
  ; [ principal_header, principal; scopes_header, "" ]
  ; proxy_headers principal @ [ "X-E2E-Principal", principal ]
  ; proxy_headers principal @ [ "X-E2E-Scopes", "prompt.list" ]
  ; [ principal_header, principal; scopes_header, "prompt.list prompt.list" ]
  ; [ principal_header, principal; scopes_header, "invalid-scope-" ^ secret ]
  ; [ principal_header, principal; scopes_header, ", ," ]
  ; proxy_headers ("invalid-id-" ^ secret)
  ]
;;

let test_proxy_headers env environment =
  with_proxy env environment "proxy-headers" "127.0.0.1" (fun sw fixture _daemon ->
    let principal =
      Agent_protocol.Id.Principal.create () |> Agent_protocol.Id.Principal.to_string
    in
    List.iter (proxy_errors principal) ~f:(fun headers ->
      raw_initialize ~sw env fixture headers |> denied;
      raw_initialize
        ~sw
        env
        fixture
        (headers @ bearer (Config_fixture.admin_token fixture))
      |> denied);
    raw_initialize
      ~sw
      env
      fixture
      (proxy_headers principal @ [ "authorization", "Basic secret" ])
    |> denied)
;;

let require_static_identity ~sw env fixture principal headers =
  let response =
    raw_initialize ~sw env fixture (headers @ bearer (Config_fixture.admin_token fixture))
  in
  let actual = accepted response "http.static_bearer" in
  require
    (not (String.equal (Agent_protocol.Id.Principal.to_string actual.id) principal))
    "untrusted proxy overrode bearer identity"
;;

let test_proxy_spoofing env environment =
  with_proxy env environment "proxy-spoofing" "192.0.2.1" (fun sw fixture _daemon ->
    let principal =
      Agent_protocol.Id.Principal.create () |> Agent_protocol.Id.Principal.to_string
    in
    let spoofed =
      proxy_headers principal
      @ [ "x-forwarded-for", "192.0.2.1"
        ; "forwarded", "for=192.0.2.1"
        ; "x-real-ip", "192.0.2.1"
        ]
    in
    raw_initialize ~sw env fixture spoofed |> denied;
    List.iter
      [ spoofed; List.hd_exn (proxy_errors principal) ]
      ~f:(require_static_identity ~sw env fixture principal))
;;

let config_ok = function
  | Ok value -> value
  | Error errors ->
    raise_s
      [%sexp "configuration failed", (errors : Agent_server.Config.Diagnostic.t list)]
;;

let load_config env fixture =
  let path = Config_fixture.config_path fixture in
  let raw = Agent_server.Config_parser.load ~env ~path |> config_ok in
  Agent_server.Config_validator.validate ~env raw |> config_ok
;;

let require_startup_denied env environment config oauth_resolver =
  let roots = Temporary_environment.roots environment in
  Eio.Switch.run (fun sw ->
    let options = { Agent_server.Daemon.default_options with oauth_resolver } in
    match
      Agent_server.Daemon.start
        ~sw
        ~env
        ~config
        ~tool_dir:roots.root
        ~home:roots.home
        ~process_start_identity:None
        ~options
        ()
    with
    | Ok daemon ->
      let (_ : (unit, Agent_protocol.Error.t) result) =
        Agent_server.Daemon.shutdown daemon
      in
      require false "missing OAuth resolver allowed daemon startup"
    | Error error ->
      require
        (Agent_protocol.Error.equal_code error.code Unauthenticated)
        "missing resolver returned wrong startup error")
;;

let test_resolver_unavailable env environment =
  let fixture = fixture env environment "resolver-unavailable" in
  configure fixture "(oauth_validator e2e-oauth)";
  let config = load_config env fixture in
  List.iter
    [ None; Some (fun _ -> None) ]
    ~f:(require_startup_denied env environment config)
;;

let catalog_ids client =
  let page = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok in
  let prompts =
    Http_driver.request
      client
      (Prompt_list { page; enabled = Some true; available = Some true })
    |> protocol_ok
  in
  let workspaces =
    Http_driver.request
      client
      (Workspace_list { page; kind = None; access = None; available = Some true })
    |> protocol_ok
  in
  match prompts.result, workspaces.result with
  | Prompt_list prompts, Workspace_list workspaces ->
    let workspace =
      List.find_exn workspaces.items ~f:(fun item -> String.equal item.name "physical")
    in
    (List.hd_exn prompts.items).id, workspace.id
  | _ -> failwith "unexpected catalog result"
;;

let create_request client =
  let prompt, workspace = catalog_ids client in
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt)
      ~workspace:(Configured workspace)
      ~liveness:Detached
      ~persistence:Durable
      ~permission_profile:"unattended"
      ~start_immediately:false
      ~labels:[ "suite", "auth-security" ]
      ()
    |> protocol_ok
  in
  Agent_protocol.Session.Create_request.
    { spec
    ; requested_mode = None
    ; subscribe = false
    ; idempotency_key =
        Agent_protocol.Idempotency_key.of_string "auth-denial" |> protocol_ok
    }
;;

let rpc_body command =
  let id = Agent_protocol.Envelope.Request_id.of_json (`Number "2") |> protocol_ok in
  Agent_protocol.Envelope.request
    ~id
    ~method_:(Agent_protocol.Command.method_name command)
    ~params:(Agent_protocol.Command.params command)
    ()
  |> Agent_protocol.Envelope.to_json
  |> Jsonaf.to_string
;;

let require_no_sessions env daemon =
  require
    (List.is_empty
       (Agent_server.Session_registry.summaries (Agent_server.Daemon.registry daemon)))
    "scope denial mutated session registry";
  require
    (List.is_empty
       (Agent_server.Daemon.store daemon |> Agent_store.Session_store.list_sessions))
    "scope denial mutated durable session index";
  let directory =
    Agent_server.Daemon.store daemon
    |> Agent_store.Session_store.data_root
    |> Agent_store.Data_root.sessions_path
  in
  require
    (List.is_empty (Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory)))
    "scope denial left a session directory"
;;

let denied_create ~sw env fixture request headers =
  with_client ~sw env fixture None (fun client ->
    let response = Http_driver.rpc_raw client ~headers initialize_body |> http_ok in
    require (response.status = 200) "restricted principal could not initialize";
    let response =
      Http_driver.rpc_raw client ~headers (rpc_body (Session_create request)) |> http_ok
    in
    require (response.status = 200) "scope denial should be an RPC error";
    let error =
      Jsonaf.of_string response.body
      |> Jsonaf.member_exn "error"
      |> Agent_protocol.Error.of_json
      |> protocol_ok
    in
    require
      (Agent_protocol.Error.equal_code error.code Permission_denied)
      "create did not deny scope")
;;

let authorized_create admin daemon request =
  let result = Http_driver.request admin (Session_create request) |> protocol_ok in
  match result.result with
  | Session_create _ ->
    require
      (List.length
         (Agent_server.Daemon.store daemon |> Agent_store.Session_store.list_sessions)
       = 1)
      "authorized control did not persist exactly one session"
  | _ -> require false "authorized create returned wrong result"
;;

let exercise_scope_denial ~sw env fixture daemon principal admin =
  let (_ : Agent_protocol.Initialize.Response.t * Http_driver.response) =
    Http_driver.initialize admin |> protocol_ok
  in
  let request = create_request admin in
  let credentials =
    [ bearer (Config_fixture.public_token fixture)
    ; bearer "oauth-valid"
    ; proxy_headers
        (Agent_protocol.Id.Principal.to_string principal.Agent_protocol.Principal.id)
    ]
  in
  List.iter credentials ~f:(fun headers ->
    denied_create ~sw env fixture request headers;
    require_no_sessions env daemon);
  authorized_create admin daemon request
;;

let test_scope_denial env environment =
  let fixture = fixture env environment "scope-denial" in
  configure
    fixture
    (sprintf
       "(oauth_validator e2e-oauth) (reverse_proxy ((trusted_addresses (127.0.0.1)) \
        (principal_header %s) (scopes_header %s)))"
       principal_header
       scopes_header);
  let principal = oauth_principal () in
  Daemon_host.with_ env fixture ~options:(oauth_options principal) (fun sw daemon ->
    with_client
      ~sw
      env
      fixture
      (Some (Config_fixture.admin_token fixture))
      (exercise_scope_denial ~sw env fixture daemon principal))
;;

let scope_key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok

let scope_attach client session_id after_sequence key =
  (Http_driver.request
     client
     (Session_attach
        { session_id
        ; requested_mode = Read_only
        ; subscribe = true
        ; after_sequence
        ; reclaim_token = None
        ; idempotency_key = scope_key key
        })
   |> protocol_ok)
    .result
;;

let require_no_private_schedule json =
  require
    (not
       (String.is_substring (Jsonaf.to_string json) ~substring:"private-scope-schedule"))
    "private schedule appeared in projection"
;;

let rec await_hidden_schedule env stream =
  let event =
    Http_driver.Sse.next stream ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:5.
    |> http_ok
  in
  match event.event with
  | Some "session.event" ->
    let durable =
      Jsonaf.of_string event.data |> Agent_protocol.Event.Durable.of_json |> protocol_ok
    in
    require_no_private_schedule (Agent_protocol.Event.Durable.to_json durable);
    if Agent_protocol.Event.Durable.equal_kind durable.kind Schedule_created
    then
      require
        (Agent_protocol.Event.Durable.equal_visibility durable.visibility Hidden)
        "schedule event was not hidden"
    else await_hidden_schedule env stream
  | _ -> await_hidden_schedule env stream
;;

let test_scope_projection_clients ~sw ~token env admin reader =
  let create = { (create_request admin) with requested_mode = Some Read_write } in
  let created =
    (Http_driver.request admin (Session_create create) |> protocol_ok).result
  in
  let created =
    match created with
    | Session_create value -> value
    | _ -> failwith "create"
  in
  let session_id = created.session.id in
  let writer_id = (Option.value_exn created.attachment).attachment.id in
  let initial, _ = Http_driver.get_snapshot admin session_id |> http_ok in
  ignore
    (scope_attach reader session_id None "scoped-attach" : Agent_protocol.Method_result.t);
  let stream, _ = Http_driver.open_session_events reader ~sw ~session_id () |> http_ok in
  Exn.protect
    ~finally:(fun () -> Http_driver.Sse.close stream)
    ~f:(fun () ->
      ignore
        (Http_driver.request
           admin
           (Schedule_create
              { session_id
              ; attachment_id = writer_id
              ; payload = `String "private-scope-schedule"
              ; due = After_ms 3600000
              ; misfire = Deliver_once_immediately
              ; idempotency_key = scope_key "scoped-schedule"
              })
         |> protocol_ok
         : Http_driver.rpc_response);
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        await_hidden_schedule env stream));
  let scoped, scoped_response = Http_driver.get_snapshot reader session_id |> http_ok in
  require (List.is_empty scoped.schedules) "HTTP snapshot exposed schedules";
  let full, full_response = Http_driver.get_snapshot admin session_id |> http_ok in
  require (List.length full.schedules = 1) "admin schedule missing";
  require
    (not
       (Option.equal
          String.equal
          (Piaf.Headers.get scoped_response.headers "etag")
          (Piaf.Headers.get full_response.headers "etag")))
    "ETag ignored projection scopes";
  scope_attach reader session_id (Some initial.latest_event_sequence) "scoped-replay"
  |> Agent_protocol.Method_result.to_json
  |> require_no_private_schedule;
  (Http_driver.request reader (Session_get { session_id; history = None }) |> protocol_ok)
    .result
  |> Agent_protocol.Method_result.to_json
  |> require_no_private_schedule;
  let blob =
    (Http_driver.request
       admin
       (Session_export
          { session_id
          ; attachment_id = writer_id
          ; format = Json
          ; revision = None
          ; history = None
          })
     |> protocol_ok)
      .result
  in
  let blob =
    match blob with
    | Session_export value -> value.blob
    | _ -> failwith "export"
  in
  let response =
    Http_driver.request_raw
      reader
      ~headers:(bearer token)
      ~meth:`GET
      ~path:("/v1/blobs/" ^ Agent_protocol.Id.Blob.to_string blob.id)
      ()
    |> http_ok
  in
  require
    (response.status = 403)
    (sprintf "restricted export HTTP status=%d" response.status)
;;

let assert_connection_authority_rejected env ~token client =
  let check response =
    let response = http_ok response in
    require
      (response.Http_driver.status = 400)
      (sprintf "connection authority mismatch returned HTTP %d" response.status);
    let error =
      Agent_protocol.Error.of_json (Jsonaf.of_string response.body) |> protocol_ok
    in
    require
      (Agent_protocol.Error.equal_code error.code Permission_denied)
      "wrong connection authority error"
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
    Http_driver.rpc_raw
      client
      {|{"jsonrpc":"2.0","id":"binding","method":"prompt.list","params":{"limit":1}}|}
    |> check;
    let headers =
      bearer token
      @ [ "ochat-connection-id", Option.value_exn (Http_driver.connection_id client) ]
    in
    Http_driver.request_raw client ~headers ~meth:`GET ~path:"/v1/events" () |> check;
    Http_driver.close_connection client |> check)
;;

let verify_connection_binding env ~token identity principal admin reader =
  let own_connection = Http_driver.connection_id reader in
  Http_driver.set_connection_id reader (Http_driver.connection_id admin);
  let variants =
    [ { principal with
        Agent_protocol.Principal.scopes =
          Agent_protocol.Scope.Set.singleton View_session_transcript
      }
    ; { principal with authentication_kind = "http.changed" }
    ; { principal with attributes = [ "policy", "changed" ] }
    ; { principal with id = Agent_protocol.Id.Principal.create () }
    ]
  in
  List.iter variants ~f:(fun value ->
    identity := Some value;
    assert_connection_authority_rejected env ~token reader);
  ignore
    (Http_driver.request admin (Protocol_ping { payload = None }) |> protocol_ok
     : Http_driver.rpc_response);
  Http_driver.set_connection_id reader own_connection;
  identity := Some (List.hd_exn variants)
;;

let test_scope_projection env environment =
  let fixture = fixture env environment "scope-projection" in
  configure fixture "(oauth_validator e2e-scoped)";
  let token = Agent_protocol.Id.Transaction.(to_string (create ())) in
  Temporary_environment.register_secret environment token;
  let identity = ref None in
  let options =
    { Agent_server.Daemon.default_options with
      oauth_resolver =
        Some
          (fun _ ->
            Some
              (fun ~now:_ ~token:presented ->
                match !identity with
                | Some principal when String.equal token presented -> Ok principal
                | _ -> Error (auth_error Unauthenticated "invalid scoped test token")))
    }
  in
  Daemon_host.with_ env fixture ~options (fun sw _ ->
    with_client
      ~sw
      env
      fixture
      (Some (Config_fixture.admin_token fixture))
      (fun admin ->
         let initialized, _ = Http_driver.initialize admin |> protocol_ok in
         require
           (initialized.event_retention.maximum_events = 1_000)
           "initialize did not advertise configured event replay capacity";
         identity
         := Some
              { initialized.principal with
                scopes = Agent_protocol.Scope.Set.singleton View_session_transcript
              };
         with_client ~sw env fixture (Some token) (fun reader ->
           ignore
             (Http_driver.initialize reader |> protocol_ok
              : Agent_protocol.Initialize.Response.t * Http_driver.response);
           verify_connection_binding
             env
             ~token
             identity
             initialized.principal
             admin
             reader;
           test_scope_projection_clients ~sw ~token env admin reader)))
;;

let cases =
  [ "oauth.valid", test_oauth_valid
  ; "oauth.invalid", test_oauth_denied "oauth-invalid"
  ; "oauth.expired", test_oauth_denied "oauth-expired"
  ; "oauth.throwing", test_oauth_denied "oauth-throwing"
  ; "oauth.unavailable", test_oauth_denied "oauth-unavailable"
  ; "oauth.resolver-unavailable", test_resolver_unavailable
  ; "callback.exception", test_callback_exception
  ; "callback.cancellation", test_callback_cancellation
  ; "static.headers", test_static_headers
  ; "static.expired", test_static_expired
  ; "proxy.trusted", test_proxy_trusted
  ; "proxy.headers", test_proxy_headers
  ; "proxy.spoofing", test_proxy_spoofing
  ; "scope.before-mutation", test_scope_denial
  ; "scope.snapshot-replay-live-export", test_scope_projection
  ]
;;

let select = function
  | None -> cases
  | Some name -> [ name, List.Assoc.find_exn cases name ~equal:String.equal ]
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"auth-security" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn -> raise_s [%sexp "auth/security case failed", (name : string), (exn : Exn.t)]);
    let report =
      [%sexp
        { scenario = ("auth-security" : string)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }]
    in
    Eio.Flow.copy_string (Sexp.to_string_hum report ^ "\n") (Eio.Stdenv.stdout env))
;;
