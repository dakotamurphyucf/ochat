open! Core
open Agent_server_test_support
module P = Agent_protocol
module H = Piaf
module A = Agent_session.Session_actor
module R = Agent_server.Session_registry

let attachments actor =
  (A.state actor |> protocol_ok).attachments
  |> List.map ~f:(fun attachment -> attachment.P.Session.Attachment.id)
  |> List.sort ~compare:P.Id.Attachment.compare
;;

let start_http ~sw ~env ~daemon ~principal =
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
    Eio.Switch.run (fun server_sw ->
      Agent_transport_http.Server.run
        ~sw:server_sw
        ~env
        ~address
        ~dispatcher:(Agent_server.Daemon.dispatcher daemon)
        ~registry:(Agent_server.Daemon.registry daemon)
        ~blob_store:(Agent_server.Daemon.blob_store daemon)
        ~health:(Agent_server.Daemon.health daemon)
        ~close_connection:(Agent_server.Daemon.close_connection daemon)
        ~authenticate:(fun _ _ -> Ok principal)
        ~max_body_bytes:1048576
        ~max_batch_size:16
        ~batch_concurrency:8
        ~outgoing_capacity:16
        ~max_connections:8
        ~max_attachments:8
          (* A 15-second heartbeat cannot account for EOF within the two-second
           assertion below; projection rejection must terminate the stream. *)
        ~idle_connection_timeout:60.
        ~on_error:raise);
    `Stop_daemon);
  let rec ready () =
    match
      Eio.Switch.run (fun probe_sw ->
        Eio.Net.connect ~sw:probe_sw (Eio.Stdenv.net env) address |> Eio.Flow.close)
    with
    | () -> ()
    | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) ->
      Eio.Fiber.yield ();
      ready ()
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. ready;
  match address with
  | `Tcp (_, port) -> port
  | `Unix _ -> assert false
;;

let%expect_test "HTTP session projection rejection closes queued replay and attachment" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:false Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let prompt_file = Filename.concat root "prompt.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Offline HTTP projection regression.</developer>";
        Eio.Switch.run (fun sw ->
          let daemon =
            Agent_server.Daemon.start
              ~sw
              ~env
              ~config:(config root root prompt_file)
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              let principal = principal () in
              let connection = connection daemon principal in
              Exn.protect
                ~finally:(fun () -> Agent_client.Connection.close connection)
                ~f:(fun () ->
                  initialize connection;
                  let session, attachment = create_session connection in
                  ignore
                    (Agent_client.Connection.request_without_history
                       connection
                       (Session_reset
                          { session_id = session.id
                          ; attachment_id = attachment.id
                          ; expected_revision = session.revision
                          ; keep_history = true
                          ; keep_tasks = true
                          ; keep_cache = true
                          ; keep_workspace = true
                          ; keep_grants = true
                          ; keep_labels = true
                          ; idempotency_key =
                              P.Idempotency_key.of_string "http-replay-boundary"
                              |> protocol_ok
                          })
                     |> protocol_ok
                     : P.Method_result.t);
                  let registry = Agent_server.Daemon.registry daemon in
                  let entry = R.load registry session.id |> protocol_ok in
                  let snapshot = A.snapshot entry.actor |> protocol_ok in
                  assert (Int64.(snapshot.latest_event_sequence >= 2L));
                  let valid =
                    P.Event.Durable.of_payload
                      ~session_id:session.id
                      ~sequence:snapshot.latest_event_sequence
                      ~revision:snapshot.revision
                      ~timestamp:snapshot.session.updated_at
                      (Session_updated snapshot.session)
                  in
                  let malformed =
                    { valid with
                      sequence = Int64.pred valid.sequence
                    ; payload = `Object [ "private_secret", `String "must-not-leak" ]
                    }
                  in
                  assert (
                    Result.is_error
                      (Agent_server.Principal_projection.durable principal malformed));
                  assert (
                    Result.is_ok
                      (Agent_server.Principal_projection.durable principal valid));
                  (* Corrupt only the retained replay fixture. The real actor,
                     committed snapshot and persistence owner remain admitted. *)
                  let durable_events =
                    Agent_session.Durable_event_log.create
                      ~capacity:8
                      [ malformed; valid ]
                    |> protocol_ok
                  in
                  ignore (R.remove registry session.id : R.entry option);
                  R.add registry ~session_id:session.id { entry with durable_events }
                  |> protocol_ok;
                  let before = attachments entry.actor in
                  let port = start_http ~sw ~env ~daemon ~principal in
                  let uri =
                    Uri.of_string
                      (sprintf
                         "http://127.0.0.1:%d/v1/sessions/%s/events?after_sequence=%Ld"
                         port
                         (P.Id.Session.to_string session.id)
                         (Int64.pred malformed.sequence))
                  in
                  let body =
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
                      Eio.Switch.run (fun client_sw ->
                        let response =
                          H.Client.Oneshot.get
                            ~config:{ H.Config.default with allow_insecure = true }
                            ~sw:client_sw
                            env
                            uri
                          |> Result.map_error ~f:H.Error.to_string
                          |> Result.ok_or_failwith
                        in
                        assert (Piaf.Status.to_code response.status = 200);
                        H.Body.to_string response.body
                        |> Result.map_error ~f:H.Error.to_string
                        |> Result.ok_or_failwith))
                  in
                  assert (String.is_prefix body ~prefix:": connected\n\n");
                  assert (String.is_substring body ~substring:"event: snapshot.required\n");
                  assert (not (String.is_substring body ~substring:"event: session.event"));
                  assert (not (String.is_substring body ~substring:"must-not-leak"));
                  let frames =
                    String.split_lines body
                    |> List.filter ~f:(String.is_prefix ~prefix:"data: ")
                  in
                  assert (Int.equal (List.length frames) 1);
                  let failure =
                    List.hd_exn frames
                    |> String.chop_prefix_exn ~prefix:"data: "
                    |> Jsonaf.of_string
                    |> P.Error.of_json
                    |> protocol_ok
                  in
                  assert (P.Error.equal_code failure.code Snapshot_required);
                  [%test_eq: P.Id.Attachment.t list] before (attachments entry.actor))))));
  print_endline
    "connected; sanitized snapshot.required; EOF before queued replay; attachment closed";
  [%expect
    {| connected; sanitized snapshot.required; EOF before queued replay; attachment closed |}]
;;
