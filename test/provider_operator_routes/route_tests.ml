open! Core
module F = Fixture
module P = Agent_protocol
module DTO = P.Provider_operator

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let internal = function
  | P.Public.Result.Non_history value -> P.Public.Result.Non_history.value value
  | _ -> failwith "expected nonprivate result"
;;

let no_challenge json =
  let encoded = Jsonaf.to_string json in
  assert (not (String.is_substring encoded ~substring:"ROUTE-PRIVATE-CODE"));
  assert (not (String.is_substring encoded ~substring:"route-device"));
  assert (not (String.is_substring encoded ~substring:"synthetic-route-code"))
;;

let denied = function
  | Error { P.Error.code = Permission_denied; _ } -> ()
  | Error error -> raise_s [%sexp (error : P.Error.t)]
  | Ok _ -> failwith "unauthorized provider operation admitted"
;;

let request client command = F.Client.request client command

let setup client =
  request client (Provider_setup { idempotency_key = F.key "route-setup" })
  |> ok
  |> internal
  |> function
  | P.Method_result.Provider_setup _ -> ()
  | _ -> failwith "wrong setup response"
;;

let begin_request =
  P.Command.Provider_login_begin
    { DTO.Login_request.profile = F.profile
    ; mode = Device
    ; idempotency_key = F.key "original-route-login"
    }
;;

let begin_flow client =
  request client begin_request
  |> ok
  |> internal
  |> function
  | P.Method_result.Provider_login_begin flow -> flow
  | _ -> failwith "wrong login response"
;;

let status client =
  let result = request client (Provider_status { profile = None }) |> ok in
  no_challenge (P.Public.Result.to_json result);
  match internal result with
  | P.Method_result.Provider_status value -> value
  | _ -> failwith "wrong status"
;;

let receipt client =
  let result =
    request
      client
      (Command_receipt
         { method_name = P.Command.method_name begin_request
         ; original_params = P.Command.params begin_request
         })
    |> ok
  in
  no_challenge (P.Public.Result.to_json result);
  match internal result with
  | P.Method_result.Command_receipt value -> value
  | _ -> failwith "wrong receipt"
;;

let same_flow expected actual =
  assert (P.Id.Server.equal expected.DTO.Flow_ref.server_id actual.DTO.Flow_ref.server_id);
  assert (DTO.Flow_id.equal expected.flow_id actual.flow_id)
;;

let cancel client flow =
  request
    client
    (Provider_login_cancel { flow; idempotency_key = F.key "original-route-cancel" })
  |> ok
  |> internal
  |> function
  | P.Method_result.Provider_login_cancel value -> value
  | _ -> failwith "wrong cancel result"
;;

let private_challenge client flow =
  let result = request client (Provider_login_challenge { flow }) |> ok in
  match result with
  | P.Public.Result.Private_provider_challenge value ->
    assert (
      Option.value_exn
        (DTO.Private_challenge.with_device_prompt
           value
           ~f:(fun ~verification_uri:_ ~user_code ->
             String.equal user_code "ROUTE-PRIVATE-CODE")));
    no_challenge (`String (Sexp.to_string_hum (DTO.Private_challenge.sexp_of_t value)))
  | _ -> failwith "owner private challenge missing"
;;

let lifecycle route =
  F.with_fixture route (fun fixture ->
    let owner = F.connect fixture Owner in
    let viewer = F.connect fixture Viewer in
    let foreign = F.connect fixture Foreign in
    setup owner;
    F.assert_inference_unavailable fixture;
    denied (request viewer begin_request);
    let missing =
      P.Command.Provider_login_begin
        { DTO.Login_request.profile = DTO.Profile_id.of_string "not-declared" |> ok
        ; mode = Device
        ; idempotency_key = F.key "denied-missing-profile"
        }
    in
    denied (request viewer missing);
    assert (F.login_starts fixture = 0);
    let flow = begin_flow owner in
    F.await_poll fixture;
    assert (F.login_starts fixture = 1);
    private_challenge owner flow;
    denied (request viewer (Provider_login_challenge { flow }));
    denied (request foreign (Provider_login_challenge { flow }));
    denied
      (request
         foreign
         (Provider_login_cancel { flow; idempotency_key = F.key "foreign-cancel" }));
    assert (F.poll_exits fixture = 0 && F.exchanges fixture = 0);
    ignore (status viewer : DTO.Status_result.t);
    (match receipt owner with
     | P.Command_receipt.Committed (Provider_login actual) -> same_flow flow actual
     | _ -> failwith "original login receipt missing");
    F.Client.close owner;
    let reconnected = F.connect fixture Owner in
    let state = status reconnected in
    assert (
      List.exists state.flows ~f:(fun value ->
        DTO.Flow_id.equal value.DTO.Flow_result.flow.flow_id flow.flow_id
        && DTO.Flow_result.equal_phase value.phase Pending));
    assert (F.poll_exits fixture = 0);
    private_challenge reconnected flow;
    let cancelled = cancel reconnected flow in
    same_flow flow cancelled.flow;
    assert (DTO.Flow_result.equal_phase cancelled.phase Cancelled);
    assert (F.poll_exits fixture = 1 && F.exchanges fixture = 0);
    let original = begin_flow reconnected in
    same_flow flow original;
    assert (F.login_starts fixture = 1);
    (match receipt reconnected with
     | P.Command_receipt.Committed (Provider_login actual) -> same_flow flow actual
     | _ -> failwith "original receipt changed");
    F.Client.close reconnected;
    F.Client.close viewer;
    F.Client.close foreign);
  print_s [%sexp (route : F.route)];
  print_endline
    "owner-only challenge; redacted receipt/status; host flow survives disconnect; \
     cancel joins; original key stays original"
;;

let%expect_test "actual HTTP socket and stdio operator routes share lifecycle authority" =
  List.iter [ F.Http; Socket; Stdio ] ~f:lifecycle;
  [%expect
    {|
    Http
    owner-only challenge; redacted receipt/status; host flow survives disconnect; cancel joins; original key stays original
    Socket
    owner-only challenge; redacted receipt/status; host flow survives disconnect; cancel joins; original key stays original
    Stdio
    owner-only challenge; redacted receipt/status; host flow survives disconnect; cancel joins; original key stays original |}]
;;

let expired_actor route =
  F.with_fixture route (fun fixture ->
    let owner = F.connect fixture Owner in
    setup owner;
    let flow = begin_flow owner in
    F.await_poll fixture;
    F.expire_owner fixture;
    let renewed = F.connect fixture Renewed_owner in
    assert (F.login_starts fixture = 1 && F.poll_exits fixture = 0);
    F.release_poll fixture;
    let terminal = F.wait_terminal fixture renewed flow in
    assert (DTO.Flow_result.equal_phase terminal.phase (Failed Denied));
    assert (F.exchanges fixture = 1 && F.poll_exits fixture = 1);
    let state = status renewed in
    let selected =
      List.find_exn state.profiles ~f:(fun value ->
        DTO.Profile_id.equal value.DTO.Status_result.profile F.profile)
    in
    assert (Option.is_none selected.credential_revision);
    same_flow flow (begin_flow renewed);
    assert (F.login_starts fixture = 1);
    F.Client.close owner;
    F.Client.close renewed);
  print_s [%sexp (route : F.route)];
  print_endline
    "new same-principal authentication cannot authorize original expired flow publication"
;;

let%expect_test
    "actual route login publication retains original actor after token replacement"
  =
  List.iter [ F.Http; Socket; Stdio ] ~f:expired_actor;
  [%expect
    {|
    Http
    new same-principal authentication cannot authorize original expired flow publication
    Socket
    new same-principal authentication cannot authorize original expired flow publication
    Stdio
    new same-principal authentication cannot authorize original expired flow publication |}]
;;

let expired_flow route =
  F.with_fixture ~flow_seconds:1 route (fun fixture ->
    let owner = F.connect fixture Owner in
    setup owner;
    let flow = begin_flow owner in
    F.await_poll fixture;
    F.expire_flow fixture;
    (match request owner (Provider_login_challenge { flow }) with
     | Error { code = Invalid_state; _ } -> ()
     | _ -> failwith "expired flow challenge remained available");
    F.release_poll fixture;
    let terminal = F.wait_terminal fixture owner flow in
    assert (DTO.Flow_result.equal_phase terminal.phase Expired);
    assert (F.exchanges fixture = 1 && F.poll_exits fixture = 1);
    let selected =
      List.find_exn (status owner).profiles ~f:(fun value ->
        DTO.Profile_id.equal value.DTO.Status_result.profile F.profile)
    in
    assert (Option.is_none selected.credential_revision);
    same_flow flow (begin_flow owner);
    assert (F.login_starts fixture = 1);
    F.Client.close owner);
  print_s [%sexp (route : F.route)];
  print_endline
    "expired public flow rejects challenge and publication while original actor remains \
     current"
;;

let%expect_test "actual host flow expiry binds OAuth publication on every route" =
  List.iter [ F.Http; Socket; Stdio ] ~f:expired_flow;
  [%expect
    {|
    Http
    expired public flow rejects challenge and publication while original actor remains current
    Socket
    expired public flow rejects challenge and publication while original actor remains current
    Stdio
    expired public flow rejects challenge and publication while original actor remains current |}]
;;

let completed_login route =
  F.with_fixture route (fun fixture ->
    let owner = F.connect fixture Owner in
    setup owner;
    let flow = begin_flow owner in
    F.await_poll fixture;
    F.release_poll fixture;
    let terminal = F.wait_terminal fixture owner flow in
    if not (DTO.Flow_result.equal_phase terminal.phase Completed)
    then raise_s [%sexp "Unexpected login terminal", (terminal : DTO.Flow_result.t)];
    let selected =
      List.find_exn (status owner).profiles ~f:(fun value ->
        DTO.Profile_id.equal value.DTO.Status_result.profile F.profile)
    in
    assert (DTO.Status_result.equal_availability selected.availability Configured);
    assert (Option.value_exn selected.account |> String.equal "route-account");
    assert (Option.is_some selected.credential_revision);
    let epoch = Option.value_exn selected.auth_epoch in
    let result =
      request
        owner
        (Provider_logout
           { profile = F.profile; idempotency_key = F.key "original-route-logout" })
      |> ok
      |> internal
    in
    (match result with
     | P.Method_result.Provider_logout value ->
       assert (Int64.(value.auth_epoch > epoch));
       assert (not value.cleanup_pending)
     | _ -> failwith "wrong logout response");
    let after =
      List.find_exn (status owner).profiles ~f:(fun value ->
        DTO.Profile_id.equal value.DTO.Status_result.profile F.profile)
    in
    assert (DTO.Status_result.equal_availability after.availability Disabled);
    assert (F.login_starts fixture = 1 && F.exchanges fixture = 1);
    same_flow flow (begin_flow owner);
    assert (F.login_starts fixture = 1);
    F.Client.close owner);
  print_s [%sexp (route : F.route)];
  print_endline
    "verified exact account published; logout advances epoch and disables; original \
     login never replays"
;;

let%expect_test "actual route completion publishes exact identity and logout fences it" =
  List.iter [ F.Http; Socket; Stdio ] ~f:completed_login;
  [%expect
    {|
    Http
    verified exact account published; logout advances epoch and disables; original login never replays
    Socket
    verified exact account published; logout advances epoch and disables; original login never replays
    Stdio
    verified exact account published; logout advances epoch and disables; original login never replays |}]
;;
