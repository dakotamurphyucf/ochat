open Core

let inference_options () =
  { Agent_server.Daemon.default_options with
    inference_policy =
      Agent_server_test_support.inference_policy
        ~default_model:"fixture-model"
        ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "unexpected fixture model dispatch")
  }
;;

let () = Mirage_crypto_rng_unix.use_default ()

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let all_scopes =
  Agent_protocol.Scope.Set.of_list
    [ List_prompts
    ; List_workspaces
    ; Create_sessions
    ; View_session_transcript
    ; Send_messages
    ; Own_sessions
    ; Answer_approvals
    ; View_security_state
    ; Manage_grants
    ; Read_audit
    ; Stop_sessions
    ; Delete_sessions
    ; Administer_configuration
    ; Diagnostics
    ]
;;

let temporary_root env =
  let suffix =
    Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
  in
  let root = Filename.concat "/tmp" ("ochat-socket-test-" ^ suffix) in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
  root
;;

let with_embedded f =
  Eio_main.run (fun env ->
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>You are a socket transport test agent.</developer>";
        Eio.Switch.run (fun sw ->
          let options =
            Agent_server.Embedded.
              { prompt_file
              ; workspace
              ; tool_dir = workspace
              ; home = root
              ; data_root = None
              ; start_immediately = false
              ; permission_profile = Agent_server.Embedded.default_permission_profile
              ; attachment_mode = Read_write
              ; event_capacity = 128
              }
          in
          let embedded =
            Agent_server.Embedded.start
              ~daemon_options:(inference_options ())
              ~sw
              ~env
              options
            |> protocol_ok
          in
          Exn.protect
            ~f:(fun () -> f sw env root embedded)
            ~finally:(fun () -> Agent_server.Embedded.close embedded)))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)))
;;

let initialize_line =
  {|{"jsonrpc":"2.0","id":1,"method":"protocol.initialize","params":{"implementation":{"name":"socket-test","version":"1"},"protocol_min":{"major":2,"minor":0},"protocol_max":{"major":2,"minor":0},"features":[],"event_encodings":["json"],"max_inbound_event_bytes":1048576}}|}
;;

let%expect_test "Unix peer credentials produce one stable same-user principal" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let first, second = Eio_unix.Net.socketpair_stream ~sw () in
      let first_principal =
        Agent_transport_socket.Peer_credentials.authenticate_same_user
          ~scopes:all_scopes
          first
        |> protocol_ok
      in
      let second_principal =
        Agent_transport_socket.Peer_credentials.authenticate_same_user
          ~scopes:all_scopes
          second
        |> protocol_ok
      in
      let uid_matches =
        List.Assoc.find first_principal.attributes "unix.uid" ~equal:String.equal
        |> Option.exists ~f:(fun value ->
          String.equal
            value
            (Agent_transport_socket.Peer_credentials.effective_uid () |> Int.to_string))
      in
      let pid_valid =
        List.Assoc.find first_principal.attributes "unix.pid" ~equal:String.equal
        |> Option.for_all ~f:(fun value -> Int.of_string value > 0)
      in
      print_s
        [%sexp
          { stable =
              (Agent_protocol.Id.Principal.compare first_principal.id second_principal.id
               = 0
               : bool)
          ; authentication_kind = (first_principal.authentication_kind : string)
          ; uid_matches : bool
          ; pid_valid : bool
          }]));
  [%expect
    {|
    ((stable true) (authentication_kind unix.peer) (uid_matches true)
     (pid_valid true))
    |}]
;;

let%expect_test "socket path preparation rejects live listeners and removes stale nodes" =
  Eio_main.run (fun env ->
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let path = Filename.concat root "agent.sock" in
        let source_path = Filename.concat root "source.sock" in
        ignore
          (Result.try_with (fun () ->
             Eio.Switch.run (fun sw ->
               ignore
                 (Eio.Net.listen
                    ~sw
                    ~reuse_addr:true
                    ~backlog:4
                    (Eio.Stdenv.net env)
                    (`Unix source_path)
                  : _ Eio.Net.listening_socket);
               Eio.Path.rename
                 Eio.Path.(Eio.Stdenv.fs env / source_path)
                 Eio.Path.(Eio.Stdenv.fs env / path)))
           : (unit, exn) result);
        let stale_existed =
          Poly.equal
            (Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path))
            `Socket
        in
        let stale_removed =
          Agent_transport_socket.Server.prepare_path ~env ~socket_path:path
          |> Result.is_ok
        in
        let live_rejected =
          Eio.Switch.run (fun sw ->
            let listener =
              Eio.Net.listen
                ~sw
                ~reuse_addr:true
                ~backlog:4
                (Eio.Stdenv.net env)
                (`Unix path)
            in
            let rejected =
              Agent_transport_socket.Server.prepare_path ~env ~socket_path:path
              |> Result.is_error
            in
            ignore (listener : _ Eio.Net.listening_socket);
            rejected)
        in
        let public_parent = Filename.concat root "public" in
        Eio.Path.mkdir ~perm:0o755 Eio.Path.(Eio.Stdenv.fs env / public_parent);
        let insecure_rejected =
          Agent_transport_socket.Server.prepare_path
            ~env
            ~socket_path:(Filename.concat public_parent "agent.sock")
          |> Result.is_error
        in
        print_s
          [%sexp
            { stale_existed : bool
            ; stale_removed : bool
            ; live_rejected : bool
            ; insecure_rejected : bool
            }])
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect
    {|
    ((stale_existed true) (stale_removed true) (live_rejected true)
     (insecure_rejected true))
    |}]
;;

let%expect_test
    "socket serving authenticates before ordered NDJSON dispatch and cleans up"
  =
  with_embedded (fun sw _env _root embedded ->
    let server_flow, client_flow = Eio_unix.Net.socketpair_stream ~sw () in
    let close_count = ref 0 in
    let errors = ref [] in
    let finished, finish = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Agent_transport_socket.Server.serve
        ~dispatcher:(Agent_server.Embedded.dispatcher embedded)
        ~close_connection:(fun context ->
          Int.incr close_count;
          Agent_server.Embedded.close_connection embedded context)
        ~authenticate:(fun flow _ ->
          Agent_transport_socket.Peer_credentials.authenticate_same_user_actor
            ~scopes:all_scopes
            flow)
        ~max_line_length:4_096
        ~outgoing_capacity:16
        ~max_attachments:64
        ~on_protocol_error:(fun error -> errors := error.code :: !errors)
        server_flow
        (`Unix "socketpair");
      Eio.Promise.resolve finish ());
    Eio.Flow.copy_string (initialize_line ^ "\n") client_flow;
    Eio.Flow.shutdown client_flow `Send;
    let response =
      Eio.Buf_read.of_flow client_flow ~max_size:8_192 |> Eio.Buf_read.line
    in
    Eio.Promise.await finished;
    let initialized =
      match Jsonaf.of_string response |> Agent_protocol.Envelope.of_json with
      | Ok (Response { outcome = Ok _; _ }) -> true
      | Ok _ | Error _ -> false
    in
    print_s
      [%sexp
        { initialized : bool
        ; close_count = (!close_count : int)
        ; errors = (List.rev !errors : Agent_protocol.Error.code list)
        }]);
  [%expect {| ((initialized true) (close_count 1) (errors ())) |}]
;;

let%expect_test "socket authentication failure does not create a connection context" =
  with_embedded (fun sw _env _root embedded ->
    let server_flow, _client_flow = Eio_unix.Net.socketpair_stream ~sw () in
    let close_count = ref 0 in
    let errors = ref [] in
    Agent_transport_socket.Server.serve
      ~dispatcher:(Agent_server.Embedded.dispatcher embedded)
      ~close_connection:(fun context ->
        Int.incr close_count;
        Agent_server.Embedded.close_connection embedded context)
      ~authenticate:(fun _ _ ->
        Error
          (Agent_protocol.Error.create
             Unauthenticated
             ~message:"test denial"
             ~retryable:false
             ()))
      ~max_line_length:4_096
      ~outgoing_capacity:16
      ~max_attachments:64
      ~on_protocol_error:(fun error -> errors := error.code :: !errors)
      server_flow
      (`Unix "socketpair");
    print_s
      [%sexp
        { close_count = (!close_count : int)
        ; errors = (List.rev !errors : Agent_protocol.Error.code list)
        }]);
  [%expect {| ((close_count 0) (errors (Unauthenticated))) |}]
;;

let%expect_test "typed client close wakes its blocked socket reader" =
  with_embedded (fun sw env root embedded ->
    let socket_path = Filename.concat root "client-close.sock" in
    Agent_transport_socket.Server.prepare_path ~env ~socket_path |> protocol_ok;
    let socket =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~backlog:4
        (Eio.Stdenv.net env)
        (`Unix socket_path)
    in
    let stop, stop_server = Eio.Promise.create () in
    let serve () =
      Eio.Net.run_server
        ~stop
        ~on_error:raise
        socket
        (Agent_transport_socket.Server.serve
           ~dispatcher:(Agent_server.Embedded.dispatcher embedded)
           ~close_connection:(Agent_server.Embedded.close_connection embedded)
           ~authenticate:(fun flow _ ->
             Agent_transport_socket.Peer_credentials.authenticate_same_user_actor
               ~scopes:all_scopes
               flow)
           ~max_line_length:4_096
           ~outgoing_capacity:16
           ~max_attachments:64
           ~on_protocol_error:(fun _ -> ()))
    in
    let close_client () =
      Exn.protect
        ~f:(fun () ->
          Eio.Switch.run (fun client_sw ->
            let connection =
              Agent_transport_socket.Client.connect
                ~sw:client_sw
                ~net:(Eio.Stdenv.net env)
                ~socket_path
                ~max_line_length:4_096
                ~notification_capacity:16
            in
            Agent_client.Session_handle.initialize
              connection
              ~implementation_name:"close-test"
              ~implementation_version:"1"
            |> protocol_ok
            |> ignore;
            Agent_client.Connection.close connection))
        ~finally:(fun () -> Eio.Promise.resolve stop_server ())
    in
    let closed =
      Eio.Time.with_timeout (Eio.Stdenv.clock env) 1. (fun () ->
        Eio.Fiber.both serve close_client;
        Ok ())
    in
    print_s [%sexp (closed : (unit, [ `Timeout ]) result)]);
  [%expect {| (Ok ()) |}]
;;

let%expect_test "socket committed malformed success retains original create intent" =
  with_embedded (fun sw env root embedded ->
    let path = Filename.concat root "malformed-success.sock" in
    let listener = Eio.Net.listen ~sw ~backlog:8 (Eio.Stdenv.net env) (`Unix path) in
    let commits = ref 0 in
    let handler flow _ =
      let context =
        Agent_server.Connection_context.create
          ~connection_id:"malformed-wire"
          ~principal:(Agent_server.Embedded.principal embedded)
          ~transport:Unix_socket
          ~publish_notification:(fun _ -> ())
          ~max_attachments:8
      in
      let reader = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
      Exn.protect
        ~finally:(fun () -> Agent_server.Embedded.close_connection embedded context)
        ~f:(fun () ->
          let rec loop () =
            match Eio.Buf_read.line reader with
            | line ->
              let envelope =
                Jsonaf.of_string line |> Agent_protocol.Envelope.of_json |> protocol_ok
              in
              let response =
                Agent_server.Dispatcher.dispatch_envelope
                  (Agent_server.Embedded.dispatcher embedded)
                  ~context
                  envelope
                |> protocol_ok
                |> Option.value_exn
              in
              let response =
                match envelope, response with
                | Request request, Response reply
                  when String.equal request.method_ "session.create" ->
                  (match reply.outcome with
                   | Ok _ ->
                     incr commits;
                     Agent_protocol.Envelope.success ~id:reply.id (`Object [])
                   | Error _ -> response)
                | _ -> response
              in
              Eio.Flow.copy_string
                (Jsonaf.to_string (Agent_protocol.Envelope.to_json response) ^ "\n")
                flow;
              loop ()
            | exception End_of_file -> ()
          in
          loop ())
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server listener handler ~on_error:(function
        | Eio.Io _ -> ()
        | exn -> raise exn));
    let connection =
      Agent_transport_socket.Client.connect
        ~sw
        ~net:(Eio.Stdenv.net env)
        ~socket_path:path
        ~max_line_length:(16 * 1024 * 1024)
        ~notification_capacity:8
    in
    ignore
      (Agent_client.Session_handle.initialize
         connection
         ~implementation_name:"malformed-success-test"
         ~implementation_version:"test"
       |> protocol_ok);
    let original_snapshot =
      Agent_client.Connection.request
        (Agent_server.Embedded.connection embedded)
        (Session_get
           { session_id = Agent_server.Embedded.session_id embedded; history = None })
      |> protocol_ok
      |> function
      | Agent_protocol.Public.Result.Session_get snapshot ->
        Agent_protocol.Public.Snapshot.fields snapshot
      | _ -> failwith "unexpected snapshot"
    in
    let request =
      Agent_protocol.Session.Create_request.
        { spec = original_snapshot.session.spec
        ; requested_mode = None
        ; subscribe = false
        ; idempotency_key =
            Agent_protocol.Idempotency_key.of_string "committed-malformed-create"
            |> protocol_ok
        }
    in
    let failure =
      match Agent_client.Connection.request connection (Session_create request) with
      | Error failure -> failure
      | Ok _ -> failwith "malformed success accepted"
    in
    assert (Agent_protocol.Error.equal_code failure.code Interrupted);
    let pending = List.hd_exn (Agent_client.Connection.pending_commands connection) in
    let next_key =
      Agent_protocol.Idempotency_key.of_string "new-key-after-malformed" |> protocol_ok
    in
    assert (
      Result.is_error
        (Agent_client.Connection.request
           connection
           (Session_create { request with idempotency_key = next_key })));
    assert (Int.equal !commits 1);
    (match Agent_client.Connection.reconcile connection pending |> protocol_ok with
     | Committed (Created_session _) -> ()
     | _ -> failwith "committed receipt not reconciled");
    Agent_client.Connection.close connection;
    print_endline
      "actual socket commit; malformed success unknown; one admission; original receipt \
       reconciled");
  [%expect
    {| actual socket commit; malformed success unknown; one admission; original receipt reconciled |}]
;;
