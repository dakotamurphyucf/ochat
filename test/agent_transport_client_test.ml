open! Core

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let summarize value =
  match
    Agent_transport_client.Endpoint.create
      ~home:(Some "/users/test")
      ~bearer_token:None
      value
  with
  | Error error ->
    print_s [%sexp (error.code : Agent_protocol.Error.code), (error.message : string)]
  | Ok endpoint ->
    print_s
      [%sexp
        (Agent_transport_client.Endpoint.kind endpoint
         : Agent_transport_client.Endpoint.kind)
      , (Agent_transport_client.Endpoint.description endpoint : string)]
;;

let%expect_test "daemon endpoints validate schemes, authority, and Unix paths" =
  summarize "unix://~/.ochat/server.sock";
  summarize "http://127.0.0.1:8080/agent";
  summarize "https://agents.example.test";
  summarize "unix://relative.sock";
  summarize "http://user@example.test";
  summarize "http://example.test?token=secret";
  [%expect
    {|
    (Unix_socket unix:///users/test/.ochat/server.sock)
    (Http http://127.0.0.1:8080/agent)
    (Http https://agents.example.test)
    (Invalid_request "Unix daemon URI path must be absolute")
    (Invalid_request "HTTP daemon URI must not contain user information")
    (Invalid_request "HTTP daemon URI must not contain a query")
    |}]
;;

let temporary_root env =
  let suffix =
    Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
  in
  let root = Filename.concat "/tmp" ("ochat-endpoint-test-" ^ suffix) in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
  root
;;

let%expect_test "bearer token files use Eio and credentials stay endpoint-private" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let token_file = Filename.concat root "token" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / token_file)
          "opaque-secret\n";
        let token =
          Agent_transport_client.Endpoint.load_bearer_token ~env ~path:token_file
          |> protocol_ok
        in
        let endpoint =
          Agent_transport_client.Endpoint.create
            ~home:None
            ~bearer_token:(Some token)
            "https://agents.example.test/api"
          |> protocol_ok
        in
        let unix_result =
          Agent_transport_client.Endpoint.create
            ~home:None
            ~bearer_token:(Some token)
            "unix:///tmp/agent.sock"
        in
        print_s
          [%sexp
            { token_loaded = (String.equal token "opaque-secret" : bool)
            ; description =
                (Agent_transport_client.Endpoint.description endpoint : string)
            ; unix_error =
                (Result.map_error unix_result ~f:(fun error -> error.message)
                 |> Result.is_error
                 : bool)
            }])
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect
    {|
    ((token_loaded true) (description https://agents.example.test/api)
     (unix_error true))
    |}]
;;
