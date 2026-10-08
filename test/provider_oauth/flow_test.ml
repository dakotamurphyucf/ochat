open! Core
module O = Provider_oauth
module P = Provider_oauth_protocol

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : O.Error.t)]
;;

let policy = O.Policy.direct_codex ~expected_account:None ~callback_port:1455 () |> ok

let print_result = function
  | Ok _ -> print_endline "ready"
  | Error error -> print_s [%sexp (error : O.Error.t)]
;;

let challenge =
  {|{"device_auth_id":"synthetic-device","user_code":"1234","interval":"1"}|}
;;

let verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

let grant =
  Jsonaf.to_string
    (`Object
        [ "authorization_code", `String "synthetic-code"
        ; "code_verifier", `String verifier
        ; "code_challenge", `String "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
        ])
;;

let%expect_test "custom device pending states then one uncertain code exchange" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let polls = ref 0 in
      let exchanges = ref 0 in
      let transport =
        O.For_testing.scripted_transport
          ~clock:(Eio.Stdenv.mono_clock env)
          (fun endpoint ~body:_ ~on_possible_submission ->
             match endpoint with
             | User_code -> Ok (200, challenge)
             | Device_poll ->
               incr polls;
               if !polls <= 2
               then Ok ((if !polls = 1 then 403 else 404), "{}")
               else Ok (200, grant)
             | Token ->
               incr exchanges;
               on_possible_submission ();
               Error Connection)
      in
      let login, _ =
        O.Login.start_device
          ~transport
          ~policy
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~wall_clock:(Eio.Stdenv.clock env)
          ~maximum_wait:(Time_ns.Span.of_sec 10.)
        |> ok
      in
      print_result (O.Login.await login);
      print_result (O.Login.await login);
      O.Login.close login;
      print_s [%sexp ((!polls, !exchanges) : int * int)]));
  [%expect
    {|
    ((stage Exchange) (code Submission_uncertain))
    ((stage Exchange) (code Closed))
    (3 1)
  |}]
;;

let%expect_test "initial unsupported device route does not acquire another method" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let calls = ref 0 in
      let transport =
        O.For_testing.scripted_transport
          ~clock:(Eio.Stdenv.mono_clock env)
          (fun _ ~body:_ ~on_possible_submission:_ ->
             incr calls;
             Ok (404, "{}"))
      in
      print_result
        (O.Login.start_device
           ~transport
           ~policy
           ~sw
           ~clock:(Eio.Stdenv.mono_clock env)
           ~wall_clock:(Eio.Stdenv.clock env)
           ~maximum_wait:(Time_ns.Span.of_sec 10.));
      print_s [%sexp (!calls : int)]));
  [%expect
    {|
    ((stage Device_challenge) (code Unsupported_route))
    1
  |}]
;;

let%expect_test "explicit device cancellation joins active poll before returning" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let entered, wake = Eio.Promise.create () in
      let exited = ref false in
      let transport =
        O.For_testing.scripted_transport
          ~clock:(Eio.Stdenv.mono_clock env)
          (fun endpoint ~body:_ ~on_possible_submission:_ ->
             match endpoint with
             | User_code -> Ok (200, challenge)
             | Device_poll ->
               Fun.protect
                 ~finally:(fun () -> exited := true)
                 (fun () ->
                    Eio.Promise.resolve wake ();
                    Eio.Fiber.await_cancel ())
             | Token -> failwith "unexpected exchange")
      in
      let login, _ =
        O.Login.start_device
          ~transport
          ~policy
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~wall_clock:(Eio.Stdenv.clock env)
          ~maximum_wait:(Time_ns.Span.of_sec 10.)
        |> ok
      in
      Eio.Promise.await entered;
      O.Login.close login;
      print_s [%sexp (!exited : bool)];
      print_result (O.Login.await login)));
  [%expect
    {|
    true
    ((stage Exchange) (code Closed))
  |}]
;;

let jwt fields =
  let encode json =
    Base64.encode_exn
      ~pad:false
      ~alphabet:Base64.uri_safe_alphabet
      (Jsonaf.to_string json)
  in
  encode (`Object [ "alg", `String "RS256" ])
  ^ "."
  ^ encode (`Object fields)
  ^ ".synthetic-signature"
;;

let tokens
      env
      ?(id_audience = "app_EMoamEEZ73f0CkXaXp7hrann")
      ?(account = "synthetic-account")
      ?(access_account = "synthetic-account")
      ?(lifetime = 3600L)
      ?now
      ?nonce
      ()
  =
  let now =
    Option.value now ~default:(Eio.Time.now (Eio.Stdenv.clock env)) |> Int64.of_float
  in
  let number n = `Number (Int64.to_string n) in
  let fields account audience =
    [ "iss", `String "https://auth.openai.com"
    ; "sub", `String "synthetic-subject"
    ; "aud", `String audience
    ; "iat", number now
    ; "exp", number Int64.(now + lifetime)
    ; "https://api.openai.com/auth", `Object [ "chatgpt_account_id", `String account ]
    ]
  in
  let id =
    jwt
      (fields account id_audience
       @ Option.to_list (Option.map nonce ~f:(fun n -> "nonce", `String n)))
  in
  let access = jwt (fields access_account "opaque-provider-declared-audience") in
  Jsonaf.to_string
    (`Object
        [ "token_type", `String "Bearer"
        ; "access_token", `String access
        ; "id_token", `String id
        ; "scope", `String "openid profile email offline_access"
        ; "expires_in", `Number "3600"
        ; "refresh_token", `String "synthetic-refresh"
        ])
;;

let%expect_test
    "authenticated exchange validates client and account without guessing access audience"
  =
  Eio_main.run (fun env ->
    List.iter
      [ tokens env ()
      ; tokens env ~id_audience:"wrong-client" ()
      ; tokens env ~access_account:"wrong-account" ()
      ]
      ~f:(fun response ->
        Eio.Switch.run (fun sw ->
          let transport =
            O.For_testing.scripted_transport
              ~clock:(Eio.Stdenv.mono_clock env)
              (fun endpoint ~body:_ ~on_possible_submission ->
                 match endpoint with
                 | User_code -> Ok (200, challenge)
                 | Device_poll -> Ok (200, grant)
                 | Token ->
                   on_possible_submission ();
                   Ok (200, response))
          in
          let login, _ =
            O.Login.start_device
              ~transport
              ~policy
              ~sw
              ~clock:(Eio.Stdenv.mono_clock env)
              ~wall_clock:(Eio.Stdenv.clock env)
              ~maximum_wait:(Time_ns.Span.of_sec 10.)
            |> ok
          in
          print_result (O.Login.await login))));
  [%expect
    {|
    ready
    ((stage Identity) (code Identity_mismatch))
    ((stage Identity) (code Identity_mismatch))
  |}]
;;

let%expect_test
    "wrong browser state does not consume legitimate callback; nonce binds trusted ID \
     token"
  =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let nonce = ref None in
      let exchanges = ref 0 in
      let transport =
        O.For_testing.scripted_transport
          ~clock:(Eio.Stdenv.mono_clock env)
          (fun endpoint ~body:_ ~on_possible_submission ->
             match endpoint with
             | User_code | Device_poll -> failwith "unexpected device request"
             | Token ->
               incr exchanges;
               on_possible_submission ();
               Ok (200, tokens env ?nonce:!nonce ()))
      in
      let login, challenge =
        O.Login.start_browser
          ~transport
          ~policy
          ~sw
          ~net:(Eio.Stdenv.net env)
          ~secure_random:(Eio.Stdenv.secure_random env)
          ~clock:(Eio.Stdenv.mono_clock env)
          ~wall_clock:(Eio.Stdenv.clock env)
          ~maximum_wait:(Time_ns.Span.of_sec 10.)
        |> ok
      in
      let uri = O.Challenge.with_browser_uri challenge ~f:Fn.id |> ok in
      nonce := Uri.get_query_param uri "nonce";
      let state = Uri.get_query_param uri "state" |> Option.value_exn in
      let send state =
        Eio.Time.Timeout.run_exn
          (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 3.)
          (fun () ->
             Eio.Switch.run (fun request_sw ->
               let flow =
                 Eio.Net.connect
                   ~sw:request_sw
                   (Eio.Stdenv.net env)
                   (`Tcp (Eio.Net.Ipaddr.V4.loopback, 1455))
               in
               Eio.Flow.copy_string
                 ("GET /auth/callback?code=synthetic-code&state="
                  ^ Uri.pct_encode state
                  ^ " HTTP/1.1\r\nHost: 127.0.0.1:1455\r\nConnection: close\r\n\r\n")
                 flow;
               let reader = Eio.Buf_read.of_flow flow ~max_size:1024 in
               print_endline (Eio.Buf_read.line reader)))
      in
      send "wrong-state";
      send state;
      let verified = O.Login.await login |> ok in
      print_endline (O.Verified.account verified);
      print_s [%sexp (!exchanges : int)];
      print_result
        (O.Direct_headers.with_headers
           verified
           ~endpoint:"https://api.openai.com/v1/responses"
           ~f:Fn.id);
      O.Direct_headers.with_headers
        verified
        ~endpoint:"https://chatgpt.com/backend-api/codex/responses"
        ~f:(fun headers -> print_s [%sexp (List.map headers ~f:fst : string list)])
      |> ok));
  [%expect
    {|
    HTTP/1.1 400 Bad Request
    HTTP/1.1 200 OK
    synthetic-account
    1
    ((stage Configure) (code Unsupported_route))
    (Authorization ChatGPT-Account-ID originator User-Agent)
  |}]
;;

let%expect_test "whole device deadline expires pending polling without code exchange" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let polls = ref 0 in
      let exchanges = ref 0 in
      let transport =
        O.For_testing.scripted_transport
          ~clock:(Eio.Stdenv.mono_clock env)
          (fun endpoint ~body:_ ~on_possible_submission:_ ->
             match endpoint with
             | User_code -> Ok (200, challenge)
             | Device_poll ->
               incr polls;
               Ok (403, "{}")
             | Token ->
               incr exchanges;
               Error Connection)
      in
      let login, _ =
        O.Login.start_device
          ~transport
          ~policy
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~wall_clock:(Eio.Stdenv.clock env)
          ~maximum_wait:(Time_ns.Span.of_sec 0.1)
        |> ok
      in
      print_result (O.Login.await login);
      O.Login.close login;
      print_s [%sexp ((!polls, !exchanges) : int * int)]));
  [%expect
    {|
    ((stage Exchange) (code Timed_out))
    (1 0)
  |}]
;;

let%expect_test
    "production HTTP parser tolerates duplicate cookies and rejects ambiguous framing"
  =
  let parse fields body =
    match
      O.For_testing.parse_response
        ("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n" ^ fields ^ "\r\n" ^ body)
    with
    | Ok sizes -> print_s [%sexp (sizes : int * int)]
    | Error error ->
      let tag =
        match error with
        | Invalid_http -> "invalid HTTP"
        | Body_limit -> "body limit"
        | Closed -> "closed"
        | Connection -> "connection"
        | Tls -> "TLS"
        | Timeout -> "timeout"
      in
      print_endline tag
  in
  parse "Set-Cookie: a=1\r\nSet-Cookie: b=2\r\nContent-Length: 2\r\n" "{}";
  parse "Content-Length: 2\r\nContent-Length: 2\r\n" "{}";
  parse "Content-Length: 2\r\nTransfer-Encoding: chunked\r\n" "{}";
  parse "Transfer-Encoding: chunked\r\n" "2\r\n{}\r\n0\r\n\r\n";
  parse "Content-Length: 262145\r\n" "";
  parse ("X-Unused: " ^ String.make 16385 'x' ^ "\r\nContent-Length: 2\r\n") "{}";
  [%expect
    {|
    (200 2)
    invalid HTTP
    invalid HTTP
    (200 2)
    body limit
    invalid HTTP
  |}]
;;

let modify_json body ~remove ~replace =
  match Jsonaf.parse body with
  | Ok (`Object fields) ->
    Jsonaf.to_string
      (`Object
          (List.filter fields ~f:(fun (key, _) ->
             (not (List.mem remove key ~equal:String.equal))
             && not (List.Assoc.mem replace key ~equal:String.equal))
           @ replace))
  | Ok _ | Error _ -> failwith "invalid synthetic response fixture"
;;

let%expect_test "refresh omits ID token but never erases null or accepts account changes" =
  Eio_main.run (fun env ->
    let original = tokens env () in
    let omitted =
      modify_json
        original
        ~remove:[ "id_token"; "refresh_token"; "scope"; "expires_in" ]
        ~replace:[]
    in
    let null_id = modify_json original ~remove:[] ~replace:[ "id_token", `Null ] in
    let wrong_account = tokens env ~access_account:"wrong-account" () in
    List.iter
      [ omitted, O.Refresh.Preserve_omitted
      ; omitted, O.Refresh.Require_rotated
      ; null_id, O.Refresh.Preserve_omitted
      ; wrong_account, O.Refresh.Preserve_omitted
      ]
      ~f:(fun (response, refresh_policy) ->
        Eio.Switch.run (fun sw ->
          let exchanged = ref false in
          let transport =
            O.For_testing.scripted_transport
              ~clock:(Eio.Stdenv.mono_clock env)
              (fun endpoint ~body:_ ~on_possible_submission ->
                 match endpoint with
                 | User_code -> Ok (200, challenge)
                 | Device_poll -> Ok (200, grant)
                 | Token ->
                   on_possible_submission ();
                   if !exchanged
                   then Ok (200, response)
                   else (
                     exchanged := true;
                     Ok (200, original)))
          in
          let login, _ =
            O.Login.start_device
              ~transport
              ~policy
              ~sw
              ~clock:(Eio.Stdenv.mono_clock env)
              ~wall_clock:(Eio.Stdenv.clock env)
              ~maximum_wait:(Time_ns.Span.of_sec 10.)
            |> ok
          in
          let prior = O.Login.await login |> ok in
          match
            O.Refresh.exchange
              ~transport
              ~policy
              ~refresh_policy
              ~wall_clock:(Eio.Stdenv.clock env)
              prior
          with
          | Verified verified ->
            let expiry_presence = function
              | P.Presence.Absent -> "absent"
              | Null -> "null"
              | Value seconds -> "value:" ^ Int64.to_string seconds
            in
            printf
              "source:%s prior:%s new:%s\n"
              (Sexp.to_string
                 (O.Verified.sexp_of_scope_source (O.Verified.scope_source verified)))
              (expiry_presence (O.Verified.response_expires_in prior))
              (expiry_presence (O.Verified.response_expires_in verified));
            O.Verified.with_material verified ~f:(fun ~access:_ ~refresh ->
              print_endline
                (match refresh with
                 | Absent -> "verified; refresh omitted"
                 | Null -> "unexpected null"
                 | Value _ -> "verified; refresh supplied"))
          | Definitely_not_submitted -> print_endline "unsent"
          | Authoritative_rejection -> print_endline "rejected"
          | Possibly_consumed -> print_endline "possibly consumed")));
  [%expect
    {|
    source:Prior_exact prior:value:3600 new:absent
    verified; refresh omitted
    possibly consumed
    possibly consumed
    possibly consumed
  |}]
;;
