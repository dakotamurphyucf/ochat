open Core
module Token = Oauth2_types.Token

let token_body =
  {|{"access_token":"loopback-token","token_type":"Bearer","expires_in":3600,"refresh_token":"refresh-token","scope":"tools","provider_extension":true}|}
;;

let credentials =
  Oauth2_manager.Client_secret { id = "client"; secret = "secret"; scope = Some "tools" }
;;

let metadata issuer : Oauth2_types.Metadata.t =
  { authorization_endpoint = issuer ^ "/authorize"
  ; token_endpoint = issuer ^ "/custom-token"
  ; registration_endpoint = None
  }
;;

let fetch env sw issuer =
  Oauth2_client_credentials.fetch_token
    ~env
    ~sw
    ~token_uri:(issuer ^ "/token")
    ~client_id:"client"
    ~client_secret:"secret"
    ~scope:"tools"
    ()
;;

let exchange env sw issuer =
  Oauth2_pkce_flow.exchange_token
    ~env
    ~sw
    ~meta:(metadata issuer)
    ~client_id:"client"
    ~code:"offline-code"
    ~code_verifier:"offline-verifier"
    ~redirect_uri:"http://127.0.0.1/cb"
;;

let expired_token =
  { Token.access_token = "expired"
  ; token_type = "Bearer"
  ; expires_in = 1
  ; refresh_token = Some "refresh-token"
  ; scope = None
  ; obtained_at = 0.
  }
;;

let assert_token env before result =
  let token = Result.ok_or_failwith result in
  Fixture.check
    (String.equal token.Token.access_token "loopback-token")
    "wrong access token";
  let now = Eio.Time.now (Eio.Stdenv.clock env) in
  Fixture.check
    Float.(token.obtained_at >= before && token.obtained_at <= now)
    "token acquisition timestamp was not local";
  token
;;

let wire_and_cache_decoder _env =
  let json = Jsonaf.of_string token_body in
  let token = Token.of_response_json ~obtained_at:42. json |> Result.ok_or_failwith in
  Fixture.check Float.(token.obtained_at = 42.) "wire timestamp missing";
  let stamped = Token.jsonaf_of_t token |> Token.t_of_jsonaf in
  Fixture.check Float.(stamped.obtained_at = 42.) "cache timestamp changed";
  let untrusted =
    Jsonaf.of_string
      {|{"access_token":"a","token_type":"Bearer","expires_in":3600,"obtained_at":-100}|}
  in
  let overridden =
    Token.of_response_json ~obtained_at:99. untrusted |> Result.ok_or_failwith
  in
  Fixture.check Float.(overridden.obtained_at = 99.) "trusted remote timestamp";
  ignore (Token.of_response_json ~obtained_at:0. (`Object []) |> Fixture.error : string);
  match Token.t_of_jsonaf json with
  | _ -> failwith "cache decoder accepted wire-only token"
  | exception _ -> ()
;;

let successful_exchange operation env =
  let requests = ref [] in
  Loopback.with_server
    env
    (fun request ->
       requests := request :: !requests;
       Loopback.json token_body)
    (fun sw issuer ->
       let before = Eio.Time.now (Eio.Stdenv.clock env) in
       ignore (assert_token env before (operation env sw issuer) : Token.t);
       Fixture.check (List.length !requests = 1) "unexpected token requests";
       Fixture.check
         (String.is_substring (List.hd_exn !requests).body ~substring:"grant_type=")
         "missing grant type")
;;

let failed_exchange body operation env =
  Loopback.with_server
    env
    (fun _ -> Loopback.json body)
    (fun sw issuer -> ignore (operation env sw issuer |> Fixture.error : string))
;;

let http_status env =
  Loopback.with_server
    env
    (fun _ -> { Loopback.status = 401; body = token_body })
    (fun sw issuer ->
       let message = fetch env sw issuer |> Fixture.error in
       Fixture.check
         (String.is_substring message ~substring:"401")
         "non-success status accepted as token")
;;

let discovery_fallback status body env =
  Loopback.with_server
    env
    (fun _ -> { Loopback.status; body })
    (fun sw issuer ->
       let meta =
         Oauth2_manager.fetch_metadata ~env ~sw ~issuer |> Result.ok_or_failwith
       in
       Fixture.check
         (String.equal meta.token_endpoint (issuer ^ "/token"))
         "wrong token fallback";
       Fixture.check
         (String.equal meta.authorization_endpoint (issuer ^ "/authorize"))
         "wrong authorize fallback")
;;

let discovery_transport_failure env =
  Eio.Switch.run (fun sw ->
    let listener =
      Eio.Net.listen
        ~sw
        ~backlog:1
        (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port =
      match Eio.Net.listening_addr listener with
      | `Tcp (_, port) -> port
      | _ -> assert false
    in
    Eio.Net.close listener;
    let issuer = sprintf "http://127.0.0.1:%d" port in
    let meta = Oauth2_manager.fetch_metadata ~env ~sw ~issuer |> Result.ok_or_failwith in
    Fixture.check
      (String.equal meta.token_endpoint (issuer ^ "/token"))
      "no network fallback")
;;

let refresh creds env =
  let origin = ref "" in
  let requests = ref [] in
  Loopback.with_server
    env
    (fun request ->
       requests := request :: !requests;
       if String.is_suffix request.target ~suffix:"oauth-authorization-server"
       then
         Loopback.json
           (Oauth2_types.Metadata.jsonaf_of_t (metadata !origin) |> Jsonaf.to_string)
       else Loopback.json token_body)
    (fun sw issuer ->
       origin := issuer;
       let before = Eio.Time.now (Eio.Stdenv.clock env) in
       let result =
         Oauth2_manager.refresh_access_token ~env ~sw ~issuer creds expired_token
       in
       ignore (assert_token env before result : Token.t);
       Fixture.check
         (List.exists !requests ~f:(fun request ->
            String.is_substring request.body ~substring:"grant_type=refresh_token"))
         "refresh grant not sent")
;;

let cache_roundtrip env =
  Fixture.with_cache env (fun _ ->
    let calls = ref 0 in
    Loopback.with_server
      env
      (fun _ ->
         incr calls;
         Loopback.json token_body)
      (fun sw issuer ->
         let first =
           Oauth2_manager.get ~env ~sw ~issuer credentials |> Result.ok_or_failwith
         in
         let cached =
           Oauth2_manager.load ~env issuer credentials |> Result.ok_or_failwith
         in
         Fixture.check
           Float.(cached.obtained_at = first.obtained_at)
           "cache changed timestamp";
         let second =
           Oauth2_manager.get ~env ~sw ~issuer credentials |> Result.ok_or_failwith
         in
         Fixture.check
           Float.(second.obtained_at = first.obtained_at)
           "fresh cache not reused";
         Fixture.check (!calls = 1) "fresh cache made another HTTP call"))
;;

let refresh_reacquires env =
  Fixture.with_cache env (fun _ ->
    let grants = ref [] in
    Loopback.with_server
      env
      (fun request ->
         grants := request.body :: !grants;
         if String.is_substring request.body ~substring:"grant_type=refresh_token"
         then Loopback.json "{}"
         else Loopback.json token_body)
      (fun sw issuer ->
         Oauth2_manager.store ~env issuer credentials expired_token;
         ignore
           (Oauth2_manager.get ~env ~sw ~issuer credentials |> Result.ok_or_failwith
            : Token.t);
         Fixture.check (List.length !grants = 2) "failed refresh did not reacquire";
         Fixture.check
           (String.is_substring
              (List.hd_exn !grants)
              ~substring:"grant_type=client_credentials")
           "wrong recovery grant"))
;;

let cancellation operation env =
  let entered, enter = Eio.Promise.create () in
  let cancelled = ref false in
  Loopback.with_server
    env
    (fun _ ->
       Eio.Promise.resolve enter ();
       Eio.Fiber.await_cancel ())
    (fun _sw issuer ->
       Eio.Fiber.first
         (fun () ->
            try Eio.Switch.run (fun sw -> ignore (operation env sw issuer : _)) with
            | Eio.Cancel.Cancelled _ as exn ->
              cancelled := true;
              raise exn)
         (fun () -> Eio.Promise.await entered);
       Fixture.check !cancelled "OAuth cancellation became Error or fallback")
;;

let stored_secret env =
  Fixture.with_cache env (fun _ ->
    let authorized = ref false in
    Loopback.with_server
      env
      (fun request ->
         if String.equal request.target "/token"
         then Loopback.json token_body
         else (
           authorized
           := Poly.equal
                (List.Assoc.find request.headers ~equal:String.equal "authorization")
                (Some "Bearer loopback-token");
           Loopback.json {|{"ok":true}|}))
      (fun sw issuer ->
         Oauth2_client_store.store
           ~env
           ~issuer
           { client_id = "stored-client"; client_secret = Some "stored-secret" };
         let transport = Mcp_transport_http.connect ~sw ~env (issuer ^ "/mcp") in
         Mcp_transport_http.send transport (`Object [ "id", `Number "1" ]);
         ignore (Mcp_transport_http.recv transport : Jsonaf.t);
         Fixture.check !authorized "stored secret did not produce Authorization";
         Mcp_transport_http.close transport))
;;

let cancel_discovery env sw issuer = Oauth2_manager.fetch_metadata ~env ~sw ~issuer

let cancel_refresh env sw issuer =
  Oauth2_manager.refresh_access_token ~env ~sw ~issuer credentials expired_token
;;

let cases =
  [ "oauth.wire-versus-cache-decoder", wire_and_cache_decoder
  ; "oauth.client-credentials-success", successful_exchange fetch
  ; "oauth.pkce-exchange-success", successful_exchange exchange
  ; "oauth.client-credentials-malformed-json", failed_exchange "not-json" fetch
  ; "oauth.client-credentials-invalid-token", failed_exchange "{}" fetch
  ; "oauth.pkce-malformed-json", failed_exchange "not-json" exchange
  ; "oauth.pkce-invalid-token", failed_exchange "{}" exchange
  ; "oauth.http-error-status", http_status
  ; "oauth.discovery-404", discovery_fallback 404 "{}"
  ; "oauth.discovery-malformed-json", discovery_fallback 200 "not-json"
  ; "oauth.discovery-invalid-metadata", discovery_fallback 200 "{}"
  ; "oauth.discovery-transport-failure", discovery_transport_failure
  ; "oauth.refresh-client-secret", refresh credentials
  ; "oauth.refresh-pkce", refresh (Oauth2_manager.Pkce { client_id = "client" })
  ; "oauth.cache-roundtrip-reuse", cache_roundtrip
  ; "oauth.failed-refresh-reacquires", refresh_reacquires
  ; "oauth.client-credentials-cancellation", cancellation fetch
  ; "oauth.pkce-exchange-cancellation", cancellation exchange
  ; "oauth.refresh-cancellation", cancellation cancel_refresh
  ; "oauth.discovery-cancellation", cancellation cancel_discovery
  ; "mcp.stored-secret-loopback-auth", stored_secret
  ]
;;
