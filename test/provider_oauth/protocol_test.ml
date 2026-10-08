open! Core
module P = Provider_oauth_protocol

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let callback target =
  match P.Callback.parse ~expected_state:"owned-state" ~target with
  | Ok (Code _) -> print_endline "accepted code"
  | Ok Denied -> print_endline "denied"
  | Error error -> print_s [%sexp (error : P.Error.t)]
;;

let%expect_test "RFC7636 vector and callback binding before code admission" =
  let pkce = P.Pkce.of_verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" |> ok in
  print_endline (P.Pkce.challenge pkce);
  List.iter
    [ "/auth/callback?code=synthetic&state=wrong"
    ; "/auth/callback?code=synthetic&state=owned-state"
    ; "/auth/callback?code=synthetic&state=owned-state&state=owned-state"
    ; "/auth/callback?code=synthetic&state=owned-state&error=denied"
    ; "/auth/callback?code=%00&state=owned-state"
    ; "/auth/callback?code=%GG&state=owned-state"
    ; "/auth/callback?error=access_denied&state=owned-state"
    ; "/auth/callback?code=synthetic&state=owned-state#fragment"
    ]
    ~f:callback;
  [%expect
    {|
    E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM
    State_mismatch
    accepted code
    Invalid_callback
    Invalid_callback
    Invalid_callback
    Invalid_callback
    denied
    Invalid_callback
  |}]
;;

let%expect_test "device aliases and bounded intervals are not generic RFC8628" =
  List.iter
    [ {|{"device_auth_id":"synthetic","usercode":"1234","interval":"2"}|}
    ; {|{"device_auth_id":"synthetic","user_code":"1234","usercode":"9999","interval":"2"}|}
    ; {|{"device_auth_id":"synthetic","user_code":"1234","interval":"0"}|}
    ; {|{"device_auth_id":"synthetic","user_code":"1234","interval":2}|}
    ; {|{"device_auth_id":"synthetic","user_code":"1234","interval":"99999999999999999999999"}|}
    ]
    ~f:(fun body ->
      match P.Device.decode_challenge body with
      | Ok challenge -> print_s [%sexp (P.Device.interval_seconds challenge : int)]
      | Error error -> print_s [%sexp (error : P.Error.t)]);
  [%expect
    {|
    2
    Invalid_json
    Invalid_json
    Invalid_json
    Invalid_json
  |}]
;;

let%expect_test "token presence is retained and malformed data remains redacted" =
  let presence = function
    | P.Presence.Absent -> "absent"
    | Null -> "null"
    | Value _ -> "value"
  in
  List.iter
    [ {|{"access_token":"synthetic","token_type":"Bearer"}|}
    ; {|{"access_token":"synthetic","token_type":"Bearer","refresh_token":null}|}
    ; {|{"access_token":"synthetic","token_type":"Bearer","refresh_token":"synthetic-refresh"}|}
    ; {|{"access_token":"private-sentinel","access_token":"duplicate","token_type":"Bearer"}|}
    ; {|{"access_token":"private-sentinel","token_type":"Other"}|}
    ; String.make 262145 'x'
    ]
    ~f:(fun body ->
      match P.Token.decode body with
      | Ok token -> print_endline (presence (P.Token.refresh token))
      | Error error -> print_s [%sexp (error : P.Error.t)]);
  [%expect
    {|
    absent
    null
    value
    Invalid_json
    Invalid_json
    Response_too_large
  |}]
;;
