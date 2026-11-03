open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module SD = A.Session_state_document
module Accounts = A.Goal_accounts
module T = Goal_accounting_persistence_tests

let member = Goal_catalog_tests.member
let set = Goal_catalog_tests.set

let limits_with_bytes bytes =
  D.Limits.create
    ~max_bytes:bytes
    ~max_depth:(D.Limits.max_depth document_limits)
    ~max_fields:(D.Limits.max_fields document_limits)
    ~max_nodes:(D.Limits.max_nodes document_limits)
  |> document_ok
;;

let%expect_test
    "named accounting custody preserves absence and reserves complete host growth"
  =
  Goal_run_tests.with_model (fun ~basis ~held ~admission:_ ~intent:_ ~delta _ ->
    let initial = { basis with goal_catalog = Some held } in
    let encoded =
      SD.authored initial
      |> fun carrier -> SD.encode carrier ~limits:document_limits |> document_ok
    in
    let raw = D.Document.json encoded in
    let payload = member raw "payload" in
    assert (
      match D.Json.field payload ~name:"goal_accounts" with
      | Null -> true
      | Absent | Value _ -> false);
    let absent =
      match payload with
      | `Object fields ->
        `Object
          (List.filter fields ~f:(fun (name, _) ->
             not (String.equal name "goal_accounts")))
      | _ -> assert false
    in
    let absent =
      set raw "payload" absent
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    let reencoded = SD.encode absent ~limits:document_limits |> document_ok in
    assert (
      match D.Json.field (D.Document.payload reencoded) ~name:"goal_accounts" with
      | Absent -> true
      | Null | Value _ -> false);
    let held_goal = A.Goal_catalog.find held Goal_run_tests.goal_id |> Option.value_exn in
    let accounts =
      Accounts.bootstrap Accounts.empty ~goal:held_goal ~ledger:initial.inference_ledger
      |> protocol_ok
    in
    let bound =
      T.transition
        initial
        (Batch
           [ Goal_accounts_changed accounts
           ; Goal_catalog_changed (Option.value_exn basis.goal_catalog)
           ; delta
           ])
    in
    let charged, _ = T.record bound.state in
    let state = charged.state in
    let native = SD.authored state in
    let document = SD.encode native ~limits:document_limits |> document_ok in
    let raw = D.Document.json document in
    let raw =
      set
        raw
        "payload"
        (set (member raw "payload") "future_host" (`Object [ "retained", `True ]))
    in
    let original =
      D.Document.inspect raw ~limits:document_limits
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    let state = SD.value original in
    let account_raw = Accounts.to_jsonaf (T.accounts state) |> protocol_ok in
    let account_raw = set account_raw "future_accounts" (`String "preserve") in
    let accounts = Accounts.of_jsonaf account_raw |> protocol_ok in
    let with_unknown =
      SD.with_value original { state with goal_accounts = Some accounts }
    in
    let document = SD.encode with_unknown ~limits:document_limits |> document_ok in
    let original = SD.decode ~limits:document_limits document |> document_ok in
    let payload = D.Document.payload document in
    let dropped =
      set (member payload "goal_accounts") "future_accounts" (`String "changed")
    in
    let dropped =
      set (D.Document.json document) "payload" (set payload "goal_accounts" dropped)
    in
    let incoming =
      D.Document.inspect dropped ~limits:document_limits
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    assert (Result.is_error (SD.adopt original ~limits:document_limits incoming));
    let state = SD.value original in
    let run =
      SD.terminal_capacity original ~document ~limits:document_limits
      |> document_ok
      |> A.Run_terminal_document.growth
    in
    let goal =
      A.Goal_catalog.reserved_growth (Option.value_exn state.goal_catalog) |> protocol_ok
    in
    let account = Accounts.reserved_growth (T.accounts state) |> protocol_ok in
    let account_bytes = A.Capacity_growth.encoded_bytes account in
    assert (account_bytes > 0);
    let before_account =
      A.Capacity_growth.add run goal |> protocol_ok |> A.Capacity_growth.encoded_bytes
    in
    let bytes =
      D.Json.validate_and_measure ~limits:document_limits (D.Document.json document)
      |> document_ok
    in
    let tight = limits_with_bytes (bytes + before_account + account_bytes - 1) in
    D.Document.validate document ~limits:tight |> document_ok;
    assert (Result.is_error (SD.decode ~limits:tight document));
    assert (Result.is_error (SD.encode original ~limits:tight));
    ignore
      (SD.encode
         original
         ~limits:(limits_with_bytes (bytes + before_account + account_bytes + 64))
       |> document_ok
       : D.Document.t);
    let historical = { state with goal_accounts = None } in
    let zero =
      Accounts.bootstrap
        Accounts.empty
        ~goal:held_goal
        ~ledger:historical.inference_ledger
      |> protocol_ok
    in
    assert (
      Result.is_error
        (A.Session_transition.apply
           ~now:timestamp
           historical
           ~delta:(Goal_accounts_changed zero)
           ~payloads:[]));
    print_endline
      "absence retained; owned child protected; host growth additive; historical zero \
       rejected");
  [%expect
    {| absence retained; owned child protected; host growth additive; historical zero rejected |}]
;;
