open! Core
module P = Agent_protocol
module DTO = P.Provider_operator

let ok result =
  Result.map_error result ~f:(fun (error : P.Error.t) -> error.message)
  |> Result.ok_or_failwith
;;

let profile = DTO.Profile_id.of_string "approved-profile" |> ok
let key = P.Idempotency_key.of_string "original-operation" |> ok

let flow : DTO.Flow_ref.t =
  { server_id = P.Id.Server.of_string "srv_operator" |> ok
  ; profile
  ; flow_id = DTO.Flow_id.of_string "flow-1" |> ok
  ; expires_at = P.Timestamp.of_string "2026-10-08T12:00:00Z" |> ok
  }
;;

let principal scopes =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_operator" |> ok)
    ~authentication_kind:"synthetic"
    ~scopes:(P.Scope.Set.of_list scopes)
    ~attributes:[]
  |> ok
;;

let commands =
  [ P.Command.Provider_status { profile = Some profile }, P.Scope.Provider_view
  ; Provider_setup { idempotency_key = key }, Provider_manage
  ; ( Provider_login_begin { profile; mode = Browser; idempotency_key = key }
    , Provider_manage )
  ; Provider_login_challenge { flow }, Provider_manage
  ; Provider_login_cancel { flow; idempotency_key = key }, Provider_manage
  ; Provider_logout { profile; idempotency_key = key }, Provider_manage
  ; ( Provider_select
        { profile
        ; expected_revision = DTO.Revision.of_string "selection-1" |> ok
        ; idempotency_key = key
        }
    , Provider_select )
  ; ( Provider_configure_environment
        { profile
        ; source = DTO.Source_id.of_string "approved-source" |> ok
        ; idempotency_key = key
        }
    , Provider_manage )
  ]
;;

let%expect_test "provider method scopes never inherit session or configuration authority" =
  let session_owner =
    principal [ Own_sessions; Administer_configuration; Diagnostics; View_security_state ]
  in
  List.iter commands ~f:(fun (command, required) ->
    assert (Result.is_error (Agent_server.Authorization.authorize session_owner command));
    assert (
      Result.is_ok (Agent_server.Authorization.authorize (principal [ required ]) command));
    List.iter [ P.Scope.Provider_view; Provider_manage; Provider_select ] ~f:(fun scope ->
      if not (P.Scope.equal scope required)
      then
        assert (
          Result.is_error
            (Agent_server.Authorization.authorize (principal [ scope ]) command))));
  print_endline "view, manage, and select are explicit independent scopes";
  [%expect {| view, manage, and select are explicit independent scopes |}]
;;

let%expect_test
    "private projection requires management scope and stays outside generic results"
  =
  let challenge =
    DTO.Private_challenge.browser
      ~authorization_uri:
        (Uri.of_string "https://issuer.example.test/authorize?state=private-owner-state")
    |> ok
  in
  let result = P.Method_result.Provider_login_challenge challenge in
  assert (
    Result.is_error
      (Agent_server.Principal_projection.result (principal [ Provider_view ]) result));
  let projected =
    Agent_server.Principal_projection.result (principal [ Provider_manage ]) result |> ok
  in
  (match projected with
   | P.Public.Result.Private_provider_challenge _ -> ()
   | _ -> failwith "private challenge entered generic projection");
  assert (
    not
      (String.is_substring
         (Sexp.to_string_hum (P.Public.Result.sexp_of_t projected))
         ~substring:"private-owner-state"));
  print_endline "private projection is scoped and reflection stays redacted";
  [%expect {| private projection is scoped and reflection stays redacted |}]
;;
