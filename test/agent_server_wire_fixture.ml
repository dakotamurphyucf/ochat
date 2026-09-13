open Core
open Agent_server_test_support

(* Use the production socket framing and peer authenticator against the real
   daemon dispatcher. The listener is ready before returning the client, so test
   startup does not depend on polling or an arbitrary sleep. *)
let connect_unix ~sw ~env ~daemon ~socket_path =
  Agent_transport_socket.Server.prepare_path ~env ~socket_path |> protocol_ok;
  let listener =
    Eio.Net.listen ~sw ~backlog:16 (Eio.Stdenv.net env) (`Unix socket_path)
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Net.run_server
      listener
      (Agent_transport_socket.Server.serve
         ~dispatcher:(Agent_server.Daemon.dispatcher daemon)
         ~close_connection:(Agent_server.Daemon.close_connection daemon)
         ~authenticate:(fun flow _ ->
           Agent_transport_socket.Peer_credentials.authenticate_same_user ~scopes flow)
         ~max_line_length:1048576
         ~outgoing_capacity:512
         ~max_attachments:64
         ~on_protocol_error:(fun error ->
           raise_s [%sexp (error : Agent_protocol.Error.t)]))
      ~on_error:raise);
  Agent_transport_socket.Client.connect
    ~sw
    ~net:(Eio.Stdenv.net env)
    ~socket_path
    ~max_line_length:1048576
    ~notification_capacity:512
;;

let http_connector ~sw ~env ~daemon ~root ~(principal : Agent_protocol.Principal.t) =
  let identity =
    Agent_protocol.Principal.to_json principal
    |> Jsonaf.to_string
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  in
  let token = "local-extension-transport-fixture-" ^ identity in
  let token_file = Filename.concat root ("wire-token-" ^ identity ^ ".sexp") in
  let record =
    sprintf
      "(((token_sha256 %s) (principal_id %s) (scopes (%s)) (attributes ()) (expires_at \
       none)))"
      (Digestif.SHA256.digest_string token |> Digestif.SHA256.to_hex)
      (Agent_protocol.Id.Principal.to_string principal.id)
      (Set.to_list principal.Agent_protocol.Principal.scopes
       |> List.map ~f:Agent_protocol.Scope.to_string
       |> String.concat ~sep:" ")
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / token_file)
    record;
  let authenticator =
    Agent_server.Authenticator.load_static_file ~env ~path:token_file |> protocol_ok
  in
  let address =
    Eio.Switch.run (fun reserve_sw ->
      Eio.Net.listen
        ~sw:reserve_sw
        ~backlog:1
        (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      |> Eio.Net.listening_addr)
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun sw ->
      Agent_transport_http.Server.run
        ~sw
        ~env
        ~address
        ~dispatcher:(Agent_server.Daemon.dispatcher daemon)
        ~registry:(Agent_server.Daemon.registry daemon)
        ~blob_store:(Agent_server.Daemon.blob_store daemon)
        ~health:(Agent_server.Daemon.health daemon)
        ~close_connection:(Agent_server.Daemon.close_connection daemon)
        ~authenticate:(fun _ bearer ->
          Agent_server.Authenticator.authenticate_bearer
            authenticator
            ~now:(Agent_protocol.Timestamp.now ())
            ~token:(Option.value bearer ~default:""))
        ~max_body_bytes:1048576
        ~max_batch_size:16
        ~batch_concurrency:8
        ~outgoing_capacity:512
        ~max_connections:32
        ~max_attachments:64
        ~idle_connection_timeout:60.
        ~on_error:raise);
    `Stop_daemon);
  (* Server.run owns binding. Probe transport readiness, not a session operation,
     before creating the one logical HTTP/SSE connection used by the scenario. *)
  let rec ready () =
    match
      Eio.Switch.run (fun probe_sw ->
        Eio.Net.connect ~sw:probe_sw (Eio.Stdenv.net env) address |> Eio.Flow.close)
    with
    | () -> ()
    | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      ready ()
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. ready;
  let port =
    match address with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  fun () ->
    Agent_transport_http.Client.connect
      ~sw
      ~env
      ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" port))
      ~bearer_token:(Some token)
      ~notification_capacity:512
    |> protocol_ok
;;

(* Exercise the public stdio gateway with real pipe framing and a real Unix
   upstream. This small test client serializes typed requests; it never invokes
   the daemon dispatcher directly. Its reader drains notifications independently
   of request completion, as an interactive client must. *)
let connect_stdio ~sw ~env ~daemon ~socket_path =
  let upstream = connect_unix ~sw ~env ~daemon ~socket_path in
  let input, client_output = Eio_unix.pipe sw in
  let client_input, output = Eio_unix.pipe sw in
  let gateway =
    Eio.Fiber.fork_promise ~sw (fun () ->
      Exn.protect
        ~finally:(fun () -> Eio.Flow.close output)
        ~f:(fun () ->
          Agent_transport_stdio.Gateway.run
            ~sw
            ~connection:upstream
            ~input
            ~output
            ~max_line_length:1048576
            ~outgoing_capacity:512
            ~on_error:(fun error -> raise_s [%sexp (error : Agent_protocol.Error.t)])))
  in
  let notifications = Agent_session.Mailbox.create ~capacity:512 in
  let pending = ref None in
  let next_id = ref 0 in
  let closed = ref false in
  let buffer = Eio.Buf_read.of_flow client_input ~max_size:1048576 in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec read () =
      match Eio.Buf_read.line buffer with
      | line ->
        (match
           Jsonaf.of_string line |> Agent_protocol.Envelope.of_json |> protocol_ok
         with
         | Response response ->
           let expected, resolver = Option.value_exn !pending in
           assert (Agent_protocol.Envelope.Request_id.compare expected response.id = 0);
           pending := None;
           Eio.Promise.resolve resolver response.outcome
         | Notification _ as envelope ->
           assert (Agent_session.Mailbox.try_push notifications ~priority:Normal envelope)
         | Request _ -> failwith "stdio gateway emitted a request");
        read ()
      | exception End_of_file ->
        assert (Option.is_none !pending);
        Agent_session.Mailbox.close notifications;
        `Stop_daemon
    in
    read ());
  let request command =
    Int.incr next_id;
    let id =
      Agent_protocol.Envelope.Request_id.of_json (`Number (Int.to_string !next_id))
      |> protocol_ok
    in
    let response, resolver = Eio.Promise.create () in
    assert (Option.is_none !pending);
    pending := Some (id, resolver);
    let method_ = Agent_protocol.Command.method_name command in
    Agent_protocol.Envelope.request
      ~id
      ~method_
      ~params:(Agent_protocol.Command.params command)
      ()
    |> Agent_protocol.Envelope.to_json
    |> Jsonaf.to_string
    |> fun line ->
    Eio.Flow.copy_string (line ^ "\n") client_output;
    Eio.Promise.await response
    |> Result.bind ~f:(Agent_protocol.Method_result.of_json ~method_)
  in
  Agent_client.Transport.create
    ~request
    ~next_notification:(fun () -> Agent_session.Mailbox.pop notifications)
    ~close:(fun () ->
      match !closed with
      | true -> ()
      | false ->
        closed := true;
        Eio.Flow.close client_output;
        Eio.Promise.await_exn gateway)
  |> Agent_client.Connection.create
;;
