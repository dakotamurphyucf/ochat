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

let%expect_test
    "named connection profile keeps nonsecret references and preserved document fields"
  =
  let module Profile = Agent_transport_client.Connection_profile in
  let profile =
    Profile.create
      ~home:None
      ~name:"production"
      ~endpoint:"https://agents.example.test"
      ~expected_server:
        (Some (protocol_ok (Agent_protocol.Id.Server.of_string "srv_selected")))
      ~daemon_credential_file:(Some "/private/daemon-token")
    |> protocol_ok
  in
  let document = Profile.to_document profile |> protocol_ok in
  let json =
    match Document_schema.Document.json document with
    | `Object fields -> `Object (fields @ [ "future", `Object [ "retained", `True ] ])
    | _ -> failwith "document is not an object"
  in
  let document =
    Document_schema.Document.inspect ~limits:Document_schema.Limits.default json
    |> Result.map_error ~f:(fun _ ->
      Agent_protocol.Error.invalid_request "invalid fixture")
    |> protocol_ok
  in
  let restored = Profile.of_document ~home:None document |> protocol_ok in
  let roundtrip = Profile.to_document restored |> protocol_ok in
  assert (Document_schema.Json.equal json (Document_schema.Document.json roundtrip));
  assert (String.equal (Profile.name restored) "production");
  assert (
    not (String.is_substring (Profile.description restored) ~substring:"daemon-token"));
  print_endline
    "named target; server pin; daemon credential reference; unknown document fields \
     preserved";
  [%expect
    {| named target; server pin; daemon credential reference; unknown document fields preserved |}]
;;

let%expect_test "connection profile preserves absent and null optional fields" =
  let module Profile = Agent_transport_client.Connection_profile in
  let profile =
    Profile.create
      ~home:None
      ~name:"local"
      ~endpoint:"unix:///tmp/ochat.sock"
      ~expected_server:None
      ~daemon_credential_file:None
    |> protocol_ok
  in
  let document = Profile.to_document profile |> protocol_ok in
  let null_roundtrip =
    Profile.of_document ~home:None document
    |> protocol_ok
    |> Profile.to_document
    |> protocol_ok
  in
  assert (
    Document_schema.Json.equal
      (Document_schema.Document.json document)
      (Document_schema.Document.json null_roundtrip));
  let json = Document_schema.Document.json document in
  let absent =
    match json with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           if String.equal name "payload"
           then
             ( name
             , match value with
               | `Object fields ->
                 `Object
                   (List.filter fields ~f:(fun (name, _) ->
                      not
                        (String.equal name "expected_server"
                         || String.equal name "daemon_credential_file")))
               | _ -> failwith "unexpected payload" )
           else name, value))
    | _ -> failwith "unexpected document"
  in
  let absent_document =
    Document_schema.Document.inspect ~limits:Document_schema.Limits.default absent
    |> Result.map_error ~f:(fun _ ->
      Agent_protocol.Error.invalid_request "invalid fixture")
    |> protocol_ok
  in
  let absent_roundtrip =
    Profile.of_document ~home:None absent_document
    |> protocol_ok
    |> Profile.to_document
    |> protocol_ok
  in
  assert (
    Document_schema.Json.equal absent (Document_schema.Document.json absent_roundtrip));
  print_endline "empty profile roundtrip; absent and null retained distinctly";
  [%expect {| empty profile roundtrip; absent and null retained distinctly |}]
;;
