open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module Http = Agent_transport_http

let token_record token principal scopes =
  let atom value = Sexp.Atom value in
  Sexp.List
    [ Sexp.List
        [ atom "token_sha256"
        ; atom (Digestif.SHA256.digest_string token |> Digestif.SHA256.to_hex)
        ]
    ; Sexp.List [ atom "principal_id"; atom (P.Id.Principal.to_string principal) ]
    ; Sexp.List
        [ atom "scopes"
        ; Sexp.List
            (List.map (Set.to_list scopes) ~f:(fun scope ->
               atom (P.Scope.to_string scope)))
        ]
    ; Sexp.List [ atom "attributes"; Sexp.List [] ]
    ; Sexp.List [ atom "expires_at"; atom "none" ]
    ]
;;

let%expect_test
    "HTTP bearer ingress binds producer and scope across completion and reconnect"
  =
  let uri = ref (Uri.of_string "http://127.0.0.1") in
  let model_received = ref false in
  let connect ~sw ~env ~root daemon =
    let owner = (principal ()).id in
    let token_path = Filename.concat root "ingress-tokens.sexp" in
    let records =
      [ token_record "root-token" owner scopes
      ; token_record "helper-token" owner (P.Scope.Set.of_list [ Submit_ingress ])
      ; token_record
          "observer-token"
          owner
          (P.Scope.Set.of_list [ View_session_transcript ])
      ; token_record "foreign-token" (P.Id.Principal.create ()) scopes
      ]
    in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / token_path)
      (Sexp.to_string_mach (Sexp.List records));
    let authenticator =
      Agent_server.Authenticator.load_static_file ~env ~path:token_path |> protocol_ok
    in
    let address =
      Eio.Switch.run (fun probe_sw ->
        let listener =
          Eio.Net.listen
            ~sw:probe_sw
            ~backlog:1
            (Eio.Stdenv.net env)
            (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
        in
        Eio.Net.listening_addr listener)
    in
    let port =
      match address with
      | `Tcp (_, port) -> port
      | `Unix _ -> assert false
    in
    uri := Uri.of_string (sprintf "http://127.0.0.1:%d" port);
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Switch.run (fun sw ->
        Http.Server.run
          ~sw
          ~env
          ~address
          ~dispatcher:(Agent_server.Daemon.dispatcher daemon)
          ~registry:(Agent_server.Daemon.registry daemon)
          ~blob_store:(Agent_server.Daemon.blob_store daemon)
          ~health:(Agent_server.Daemon.health daemon)
          ~close_connection:(Agent_server.Daemon.close_connection daemon)
          ~authenticate:(fun _ token ->
            match token with
            | Some token ->
              Agent_server.Authenticator.authenticate_bearer
                authenticator
                ~now:(P.Timestamp.now ())
                ~token
            | None ->
              Error
                (P.Error.create
                   Unauthenticated
                   ~message:"bearer required"
                   ~retryable:false
                   ()))
          ~max_body_bytes:(2 * 1024 * 1024)
          ~max_batch_size:16
          ~batch_concurrency:4
          ~outgoing_capacity:128
          ~max_connections:16
          ~max_attachments:16
          ~idle_connection_timeout:60.
          ~on_error:raise);
      `Stop_daemon);
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      let rec wait () =
        match
          Eio.Switch.run (fun sw ->
            ignore (Eio.Net.connect ~sw (Eio.Stdenv.net env) address))
        with
        | () -> ()
        | exception Eio.Io _ ->
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
          wait ()
      in
      wait ());
    Http.Client.connect
      ~sw
      ~env
      ~uri:!uri
      ~bearer_token:(Some "root-token")
      ~notification_capacity:128
    |> protocol_ok
  in
  with_daemon
    ~connect
    ~sources:Ingress_socket_tests.sources
    ~expect_moderator:true
    ~expected_requests:3
    ~inspect_request:(fun request _ -> if request = 3 then model_received := true)
    ~calls:[ "watch-call", "watch", `Null ]
    ~after_turn:(fun env handle entry ->
      let snapshot = H.projection handle |> Agent_client.Projection.snapshot in
      let registration_id, namespace =
        Ingress_socket_tests.registration_from_output snapshot
      in
      let request : P.Ingress.Submit_request.t =
        { session_id = H.session_id handle
        ; registration_id
        ; namespace
        ; idempotency_key = P.Idempotency_key.of_string "http-completion" |> protocol_ok
        ; payload =
            `Object [ "value", `String "ready"; "type", `String "Permission_resolved" ]
        }
      in
      let use token f =
        Eio.Switch.run (fun sw ->
          let client =
            Http.Client.connect
              ~sw
              ~env
              ~uri:!uri
              ~bearer_token:token
              ~notification_capacity:128
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_client.Connection.close client)
            ~f:(fun () -> f client))
      in
      let denied label = function
        | Error error ->
          print_s [%sexp (label : string), (error.P.Error.code : P.Error.code)]
        | Ok _ -> failwith (label ^ " accepted")
      in
      List.iter
        [ "missing bearer", None; "invalid bearer", Some "invalid-token" ]
        ~f:(fun (label, token) ->
          use token (fun client ->
            denied
              label
              (Agent_client.Connection.request
                 client
                 (Protocol_initialize (Ingress_socket_tests.initialize_request ())))));
      List.iter
        [ "missing scope", "observer-token"; "foreign producer", "foreign-token" ]
        ~f:(fun (label, token) ->
          use (Some token) (fun client ->
            initialize client;
            denied label (Agent_client.Ingress.submit client request)));
      let submit () =
        use (Some "helper-token") (fun client ->
          let initialized =
            H.initialize
              client
              ~implementation_name:"http-helper"
              ~implementation_version:"test"
            |> protocol_ok
          in
          [%test_eq: P.Scope.Set.t]
            (P.Scope.Set.of_list [ Submit_ingress ])
            initialized.principal.scopes;
          denied
            "transcript"
            (Agent_client.Connection.request
               client
               (Session_get { session_id = request.session_id; history = None }));
          Agent_client.Ingress.submit client request |> protocol_ok)
      in
      let first = submit () in
      Subscription_tests.await_subscription env entry;
      let retried = submit () in
      assert (P.Ingress.Acknowledgement.equal first retried);
      use (Some "helper-token") (fun client ->
        initialize client;
        denied
          "changed retry"
          (Agent_client.Ingress.submit
             client
             { request with payload = `Object [ "value", `String "changed" ] })))
    ~settle:(fun env entry ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        let rec wait () =
          let state = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
          match !model_received, state.active_operation with
          | true, None -> ()
          | _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            wait ()
        in
        wait ()))
    (fun state ->
       [%test_eq: int] 1 (List.length (List.hd_exn state.ingress_registrations).receipts);
       assert (List.is_empty state.permissions);
       [%test_eq: int]
         1
         (List.count state.moderator_executions ~f:(fun execution ->
            P.Moderator_execution.equal_phase execution.context.phase Internal_event));
       [%test_eq: int]
         1
         (List.count state.conversation.canonical_history ~f:(fun entry ->
            match entry.P.History.provenance with
            | Runtime_notification _ -> true
            | _ -> false));
       print_endline "one authenticated completion, one notification, one continuation");
  [%expect
    {|
    ("missing bearer" Unauthenticated)
    ("invalid bearer" Unauthenticated)
    ("missing scope" Permission_denied)
    ("foreign producer" Permission_denied)
    (transcript Permission_denied)
    (transcript Permission_denied)
    ("changed retry" Conflict)
    one authenticated completion, one notification, one continuation
    |}]
;;
