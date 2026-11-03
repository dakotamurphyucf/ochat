open! Core
open Fixture
module Child = Agent_session.Goal_accounting_document
module Accounts = Agent_session.Goal_accounts

let unbound goal =
  G.create
    ~id:(G.id goal)
    ~session:(G.session goal)
    ~creator:(G.creator goal)
    ~source:(G.source goal)
    ~objective:(G.objective goal)
    ~budget:(G.budget goal)
  |> protocol
;;

let%expect_test
    "one canonical untracked attempt or host turn cannot charge two partitions"
  =
  List.iter [ `Attempt; `Turn ] ~f:(fun kind ->
    let current = make_ledger ~attempts:1 () in
    let before =
      match kind with
      | `Attempt ->
        let next, _, _ = admit current in
        next
      | `Turn -> current
    in
    let one, episode_one = goal ~suffix:"one" () in
    let two, episode_two = goal ~suffix:"two" () in
    let accounts =
      Accounts.bootstrap Accounts.empty ~goal:(unbound one) ~ledger:before |> protocol
    in
    let accounts =
      Accounts.bootstrap accounts ~goal:(unbound two) ~ledger:before |> protocol
    in
    let after, admission =
      match kind with
      | `Attempt ->
        let after, handle, tracking = admit before in
        assert (
          match tracking with
          | L.Untracked _ -> true
          | Tracked -> false);
        after, fun episode -> A.Admission.Attempt { episode; handle; tracking }
      | `Turn ->
        let operation = operation 1 in
        let after, handle, tracking = L.admit_turn before operation |> ledger in
        after, fun episode -> A.Admission.Turn { episode; operation; handle; tracking }
    in
    let incoming =
      List.fold
        [ one, episode_one; two, episode_two ]
        ~init:accounts
        ~f:(fun accounts (goal, episode) ->
          let child = Accounts.find accounts (G.id goal) |> Option.value_exn in
          let original = Child.resolve child ~goal ~ledger:before |> protocol in
          let plan =
            A.plan original ~goal ~before ~after ~admission:(Some (admission episode))
            |> account
          in
          Accounts.apply accounts plan |> protocol)
    in
    match
      Accounts.validate_transition
        ~previous:(Some accounts)
        ~incoming:(Some incoming)
        ~before
        ~after
        ~previous_catalog:None
        ~catalog:None
    with
    | Error error ->
      assert (
        String.equal
          error.P.Error.message
          "attributed account admissions exceed canonical progress")
    | Ok () -> failwith "one canonical admission charged twice");
  print_endline "canonical progress cannot be reused across partitions";
  [%expect {| canonical progress cannot be reused across partitions |}]
;;

(* Append to test/goal_accounting/storage_tests.ml after root releases source. *)
let member json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith name
;;

let set json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "fixture object"
;;

let first json ~f =
  match json with
  | `Array (first :: rest) -> `Array (f first :: rest)
  | _ -> failwith "fixture row"
;;

let inspect json =
  D.Document.inspect json ~limits:Child.limits
  |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let child_json child = Child.to_document child |> protocol |> D.Document.json
let restored raw = Child.of_document (inspect raw) |> protocol

let%expect_test
    "stored custody resolves exact canonical handles and funds protected final folds"
  =
  let before = make_ledger ~attempts:1 () in
  let goal, episode = goal () in
  assert (Result.is_error (Child.create goal ~ledger:before));
  let original = Child.create (unbound goal) ~ledger:before |> protocol in
  let accounting = Child.resolve original ~goal ~ledger:before |> protocol in
  let after, handle, tracking = admit before in
  let plan =
    A.plan
      accounting
      ~goal
      ~before
      ~after
      ~admission:(Some (A.Admission.Attempt { episode; handle; tracking }))
    |> account
  in
  let child = Child.apply original plan |> protocol in
  let accounting = A.Plan.candidate plan in
  let current = put after handle 0L (actual_views 7L) in
  let reopened = child_json child |> restored in
  let resolved = Child.resolve reopened ~goal ~ledger:current |> protocol in
  assert (A.equal accounting resolved);
  assert (
    U.equal
      (A.usage accounting ~ledger:current |> account)
      (A.usage resolved ~ledger:current |> account));
  let raw = child_json child in
  let payload = member raw "payload" in
  let malformed _path f =
    let raw = set raw "payload" (f payload) in
    assert (Result.is_error (Child.of_document (inspect raw)))
  in
  malformed "nominal goal" (fun p -> set p "goal_id" (`String "bad"));
  malformed "aggregate count" (fun p ->
    let settled = member p "settled" in
    let metric = member settled "input" in
    set p "settled" (set settled "input" (set metric "actual_attempts" (`String "1"))));
  let wrong_handle =
    set
      raw
      "payload"
      (set
         payload
         "bindings"
         (first (member payload "bindings") ~f:(fun row ->
            set row "context_id" (`String "usage"))))
    |> restored
  in
  assert (Result.is_error (Child.resolve wrong_handle ~goal ~ledger:current));
  let ended = L.set_state current handle interrupted |> ledger in
  let next, _, _ = admit ended in
  let fold =
    A.plan accounting ~goal ~before:ended ~after:next ~admission:None |> account
  in
  let with_unknown = set raw "future_envelope" (`String "retain") in
  let with_unknown =
    set
      with_unknown
      "payload"
      (set payload "future_aggregate" (`Object [ "retained", `True ]))
    |> restored
  in
  assert (Child.can_retire with_unknown ~ordinal:(L.Handle.ordinal handle) |> protocol);
  let folded = Child.apply with_unknown fold |> protocol in
  let encoded = child_json folded in
  assert (D.Json.equal (member encoded "future_envelope") (`String "retain"));
  assert (
    D.Json.equal
      (member (member encoded "payload") "future_aggregate")
      (`Object [ "retained", `True ]));
  let folded_account = Child.resolve folded ~goal ~ledger:next |> protocol in
  assert (A.equal folded_account (A.Plan.candidate fold));
  [%test_eq: int64] (A.settled_tracked_attempts folded_account) 1L;
  let protected_child =
    set
      raw
      "payload"
      (set
         payload
         "bindings"
         (first (member payload "bindings") ~f:(fun row ->
            set row "future_binding" (`Object [ "protected", `True ]))))
    |> restored
  in
  assert (
    not (Child.can_retire protected_child ~ordinal:(L.Handle.ordinal handle) |> protocol));
  assert (Result.is_error (Child.apply protected_child fold));
  let growth =
    Child.reserved_growth child |> protocol |> Agent_session.Capacity_growth.encoded_bytes
  in
  assert (growth > 0);
  let padded target =
    let empty = set raw "payload" (set payload "future_padding" (`String "")) in
    let bytes = Jsonaf.to_string empty |> String.length in
    set
      empty
      "payload"
      (set
         (member empty "payload")
         "future_padding"
         (`String (String.make (target - bytes) 'x')))
  in
  let too_full = padded ((256 * 1024) - growth + 1) |> inspect in
  assert (Result.is_error (Child.of_document too_full));
  let funded = padded ((256 * 1024) - growth - 32) |> restored in
  ignore (Child.apply funded fold |> protocol : Child.t);
  print_endline "exact handles; aggregate invariants; unknown custody; bounded final fold";
  [%expect {| exact handles; aggregate invariants; unknown custody; bounded final fold |}]
;;

let%expect_test "native and decoded partition limits share the same admission invariant" =
  let current = make_ledger () in
  let accounts =
    List.fold (List.init 16 ~f:Fn.id) ~init:Accounts.empty ~f:(fun accounts number ->
      let goal, _ = goal ~suffix:("bounded_" ^ Int.to_string number) () in
      Accounts.bootstrap accounts ~goal:(unbound goal) ~ledger:current |> protocol)
  in
  let goal, _ = goal ~suffix:"seventeenth" () in
  assert (
    Result.is_error (Accounts.bootstrap accounts ~goal:(unbound goal) ~ledger:current));
  let raw = Accounts.to_jsonaf accounts |> protocol in
  let payload = member raw "payload" in
  let entries =
    match member payload "accounts" with
    | `Array entries -> entries
    | _ -> assert false
  in
  let too_many =
    set raw "payload" (set payload "accounts" (`Array (List.hd_exn entries :: entries)))
  in
  assert (Result.is_error (Accounts.of_jsonaf too_many));
  print_endline "native and decoded constructors reject partition seventeen";
  [%expect {| native and decoded constructors reject partition seventeen |}]
;;
