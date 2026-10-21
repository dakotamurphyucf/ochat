open! Core
module P = Agent_protocol
module A = Agent_server.Developer_method_authorization
module Actor = Operator_authorization
module Kind = Agent_store.Developer_command_receipt.Kind

let ok result =
  Result.map_error result ~f:(fun (error : P.Error.t) -> error.message)
  |> Result.ok_or_failwith
;;

let principal ?(id = "pri_developer") ?(authentication_kind = "local.test") scopes =
  P.Principal.create
    ~id:(P.Id.Principal.of_string id |> ok)
    ~authentication_kind
    ~scopes:(P.Scope.Set.of_list scopes)
    ~attributes:[]
  |> ok
;;

let actor principal current = Actor.guarded ~principal ~is_current:(fun () -> !current)

let cases =
  [ Kind.Check, P.Scope.Developer_check, "developer.check"
  ; Kind.Fixture_run, P.Scope.Developer_fixture_run, "developer.fixture.run"
  ; Kind.Live_run, P.Scope.Developer_live_run, "developer.live.run"
  ]
;;

let denied = function
  | Error (error : P.Error.t) -> P.Error.equal_code error.code P.Error.Permission_denied
  | Ok () -> false
;;

let%expect_test "developer scopes encode exactly and remain independent" =
  List.iter cases ~f:(fun (kind, scope, encoded) ->
    assert (P.Scope.equal (A.required_scope kind) scope);
    assert (String.equal (P.Scope.to_string scope) encoded);
    assert (P.Scope.equal (P.Scope.of_string encoded |> ok) scope);
    assert (P.Scope.equal (P.Scope.of_json (P.Scope.to_json scope) |> ok) scope));
  let scopes = List.map cases ~f:(fun (_, scope, _) -> scope) in
  let restored =
    P.Scope.set_of_json (P.Scope.set_to_json (P.Scope.Set.of_list scopes)) |> ok
  in
  assert (
    List.equal
      P.Scope.equal
      (Set.to_list restored)
      (Set.to_list (P.Scope.Set.of_list scopes)));
  assert (
    Result.is_error
      (P.Scope.set_of_json
         (`Array
             [ P.Scope.to_json P.Scope.Developer_check
             ; P.Scope.to_json P.Scope.Developer_check
             ])));
  assert (Result.is_error (P.Scope.of_string "developer.run"));
  List.iter cases ~f:(fun (kind, _, _) ->
    let row =
      List.map cases ~f:(fun (_, scope, _) ->
        Result.is_ok (A.authorize_principal kind ~principal:(principal [ scope ])))
    in
    print_s [%sexp (row : bool list)]);
  assert (
    denied
      (A.authorize_principal
         Kind.Live_run
         ~principal:(principal [ P.Scope.Create_sessions; P.Scope.Diagnostics ])));
  print_endline "round trips, duplicates and legacy scopes checked";
  [%expect
    {|
    (true false false)
    (false true false)
    (false false true)
    round trips, duplicates and legacy scopes checked |}]
;;

exception Guard_failure

let%test_unit "developer action rechecks both actors and exact identity" =
  let full =
    principal
      [ P.Scope.Developer_check
      ; P.Scope.Developer_fixture_run
      ; P.Scope.Developer_live_run
      ]
  in
  let original_current = ref true in
  let request_current = ref true in
  let original = actor full original_current in
  let request = actor full request_current in
  assert (Result.is_ok (A.check_now Kind.Live_run ~original ~current:request));
  original_current := false;
  assert (denied (A.check_now Kind.Live_run ~original ~current:request));
  original_current := true;
  request_current := false;
  assert (denied (A.check_now Kind.Live_run ~original ~current:request));
  request_current := true;
  let changed_id =
    actor (principal ~id:"pri_other" [ P.Scope.Developer_live_run ]) request_current
  in
  let changed_kind =
    actor
      (principal ~authentication_kind:"http.static" [ P.Scope.Developer_live_run ])
      request_current
  in
  assert (denied (A.check_now Kind.Live_run ~original ~current:changed_id));
  assert (denied (A.check_now Kind.Live_run ~original ~current:changed_kind));
  let no_scope = actor (principal [ P.Scope.Create_sessions ]) request_current in
  assert (denied (A.check_now Kind.Live_run ~original ~current:no_scope));
  assert (denied (A.check_now Kind.Live_run ~original:no_scope ~current:request));
  let broken =
    Actor.guarded ~principal:full ~is_current:(fun () -> raise Guard_failure)
  in
  let propagates =
    try
      ignore (A.check_now Kind.Live_run ~original:broken ~current:request);
      false
    with
    | Guard_failure -> true
  in
  assert propagates
;;
