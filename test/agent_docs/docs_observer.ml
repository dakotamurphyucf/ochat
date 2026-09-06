open! Core

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let key value = Agent_protocol.Idempotency_key.of_string value |> ok

let principal env root name authenticator =
  let token = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / name) in
  Agent_server.Authenticator.authenticate_bearer
    authenticator
    ~now:(Agent_protocol.Timestamp.now ())
    ~token
  |> ok
;;

let connection daemon principal =
  let notifications = Eio.Stream.create 16 in
  let context =
    Agent_server.Connection_context.create
      ~connection_id:Agent_protocol.Id.Attachment.(to_string (create ()))
      ~principal
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:16
  in
  let client =
    Agent_client.In_memory.create
      ~notifications
      ~request:
        (Agent_server.Dispatcher.dispatch_command
           (Agent_server.Daemon.dispatcher daemon)
           ~context)
      ~close:(fun () -> Agent_server.Daemon.close_connection daemon context)
  in
  ignore
    (Agent_client.Session_handle.initialize
       client
       ~implementation_name:"docs-observer"
       ~implementation_version:"1"
     |> ok
     : Agent_protocol.Initialize.Response.t);
  client
;;

let create_session client =
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog (Agent_server.Catalog_identity.prompt_definition "hello"))
      ~workspace:
        (Configured (Agent_server.Catalog_identity.workspace_definition "project"))
      ~liveness:Detached
      ~persistence:Durable
      ~start_immediately:false
      ~labels:[]
      ()
    |> ok
  in
  match
    Agent_client.Connection.request
      client
      (Session_create
         { spec
         ; requested_mode = None
         ; subscribe = false
         ; idempotency_key = key "docs-observer-create"
         })
    |> ok
  with
  | Session_create value -> value.session.id
  | _ -> failwith "expected created tutorial session"
;;

let verify_observer client session_id =
  let attachment =
    match
      Agent_client.Connection.request
        client
        (Session_attach
           { session_id
           ; requested_mode = Read_only
           ; subscribe = false
           ; after_sequence = None
           ; reclaim_token = None
           ; idempotency_key = key "docs-observer-attach"
           })
      |> ok
    with
    | Session_attach value -> value.attachment.id
    | _ -> failwith "expected observer attachment"
  in
  let snapshot = Docs_smoke.snapshot client session_id in
  assert (not (List.is_empty snapshot.canonical_history.entries));
  List.iter snapshot.canonical_history.entries ~f:(fun entry ->
    assert (Agent_protocol.History.equal_role entry.role System);
    assert (
      String.equal
        (Jsonaf.member_exn "role" entry.payload |> Jsonaf.string_exn)
        "developer"));
  match
    Agent_client.Connection.request
      client
      (Session_send_message
         { session_id
         ; attachment_id = attachment
         ; content = { kind = Plain_text; text = "must not run"; attachments = [] }
         ; idempotency_key = key "docs-observer-denied"
         })
  with
  | Error error -> assert (Agent_protocol.Error.equal_code error.code Permission_denied)
  | Ok _ -> failwith "tutorial observer unexpectedly wrote to the session"
;;

let run env root =
  let config =
    Agent_server.Config_parser.load ~env ~path:(Filename.concat root "unix.sexp")
    |> Result.bind ~f:(Agent_server.Config_validator.validate ~env)
    |> function
    | Ok value -> value
    | Error errors -> raise_s [%sexp (errors : Agent_server.Config.Diagnostic.t list)]
  in
  let auth =
    Agent_server.Authenticator.load_static_file
      ~env
      ~path:(Filename.concat root "tokens.sexp")
    |> ok
  in
  let admin = principal env root "admin.token" auth in
  let observer = principal env root "observer.token" auth in
  assert (Agent_protocol.Id.Principal.compare admin.id observer.id = 0);
  assert (
    Agent_protocol.Scope.Set.equal
      observer.scopes
      (Agent_protocol.Scope.Set.singleton View_session_transcript));
  Eio.Switch.run (fun sw ->
    let daemon =
      Agent_server.Daemon.start
        ~sw
        ~env
        ~config
        ~tool_dir:root
        ~home:root
        ~process_start_identity:None
        ()
      |> ok
    in
    Fun.protect
      ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> ok)
      (fun () ->
         let writer = connection daemon admin in
         let reader = connection daemon observer in
         Fun.protect
           ~finally:(fun () ->
             Agent_client.Connection.close reader;
             Agent_client.Connection.close writer)
           (fun () -> verify_observer reader (create_session writer))));
  Eio.Flow.copy_string
    "Generated observer credentials/session workflow PASS (offline)\n"
    (Eio.Stdenv.stdout env)
;;
