open Core
open Agent_protocol

let get = Agent_invocation_test.get
let report = Agent_invocation_test.report
let timestamp = get (Timestamp.of_string "2026-09-08T12:00:00Z")
let later = get (Timestamp.of_string "2026-09-08T13:00:00Z")

let subscription () =
  Subscription.create
    { id = get (Id.Subscription.of_string "sub_test")
    ; session_id = get (Id.Session.of_string "ses_parent")
    ; generation = 3
    ; invocation_id = get (Id.Invocation.of_string "inv_example")
    ; kind = "agent_response"
    ; created_at = timestamp
    ; deadline = later
    ; completion_schema = None
    ; wake = Request_turn
    ; ingress_capability = None
    }
  |> get
;;

let delivery () =
  Delivery.create
    { id = get (Id.Delivery.of_string "dlv_test")
    ; session_id = get (Id.Session.of_string "ses_parent")
    ; generation = 3
    ; invocation_id = Some (get (Id.Invocation.of_string "inv_example"))
    ; work = Some (Subscription (get (Id.Subscription.of_string "sub_test")))
    ; correlation = "response-test"
    ; source = Moderator
    ; completion = Succeeded (`String "result")
    ; wake = Request_turn
    ; created_at = timestamp
    }
  |> get
;;

let history_id n =
  History_entry.Id.create ~namespace:"notification" ~sequence:n |> Result.ok_or_failwith
;;

let error : Invocation.tool_error =
  { code = "delivery_failed"
  ; message = "fixture failure"
  ; retryable = true
  ; details = `Null
  }
;;

let%expect_test "subscription timer epochs and first terminal winner survive round-trip" =
  let first = subscription () in
  let armed =
    Subscription.arm
      first
      ~expected_epoch:0
      ~timer_id:(Some (get (Id.Schedule.of_string "sch_timer")))
      ~job_id:None
    |> get
  in
  report (Subscription.finish armed ~expected_epoch:0 ~now:timestamp (Succeeded `Null));
  let finished, changed =
    Subscription.finish
      armed
      ~expected_epoch:1
      ~now:timestamp
      (Succeeded (`String "done"))
    |> get
  in
  let restored = Subscription.of_json (Subscription.to_json finished) |> get in
  let repeated, changed_again =
    Subscription.finish
      restored
      ~expected_epoch:1
      ~now:later
      (Cancelled "late cancellation")
    |> get
  in
  print_s
    [%sexp
      { changed : bool
      ; changed_again : bool
      ; timer_cleared = (Option.is_none repeated.timer_id : bool)
      ; result = (repeated.result : Completion.t option)
      }];
  report (Subscription.arm restored ~expected_epoch:2 ~timer_id:None ~job_id:None);
  report (Subscription.validate_transition ~previous:(Some restored) armed);
  [%expect
    {|
    conflict
    ((changed true) (changed_again false) (timer_cleared true)
     (result ((Succeeded (String done)))))
    already_resolved
    already_resolved |}]
;;

let%expect_test "expiry and subscription codec reject impossible durable states" =
  let s = subscription () in
  report (Subscription.finish s ~expected_epoch:0 ~now:timestamp Expired);
  let expired, _ = Subscription.finish s ~expected_epoch:0 ~now:later Expired |> get in
  report (Subscription.of_json (Subscription.to_json expired));
  let replace = Agent_invocation_test.replace_field in
  report
    (Subscription.of_json
       (replace (Subscription.to_json expired) "timer_id" (`String "sch_stale")));
  report
    (Subscription.of_json
       (replace (Subscription.to_json s) "schema_version" (`Number "2")));
  report
    (Subscription.of_json
       (replace (Subscription.to_json s) "ingress_capability" (`String "ses_forged")));
  report (Subscription.validate_transition ~previous:None expired);
  [%expect
    {|
    invalid_request
    ok
    invalid_request
    incompatible_protocol
    invalid_request
    invalid_state |}]
;;

let%expect_test
    "delivery commits cannot move to another history entry or lose their result"
  =
  let d = delivery () in
  let committed = Delivery.commit d ~history_id:(history_id 0) ~now:timestamp |> get in
  let restored = Delivery.of_json (Delivery.to_json committed) |> get in
  report (Delivery.commit restored ~history_id:(history_id 0) ~now:later);
  report (Delivery.commit restored ~history_id:(history_id 1) ~now:later);
  report (Delivery.fail restored error);
  report (Delivery.validate_transition ~previous:(Some restored) d);
  print_s [%sexp (restored.context.completion : Completion.t)];
  [%expect
    {|
    ok
    conflict
    already_resolved
    invalid_state
    (Succeeded (String result)) |}]
;;

let%expect_test "delivery retries require explicit failure and obey a bound" =
  let d = delivery () in
  report (Delivery.retry d ~max_attempts:2);
  let failed = Delivery.fail d error |> get in
  report (Delivery.commit failed ~history_id:(history_id 0) ~now:later);
  let retry = Delivery.retry failed ~max_attempts:2 |> get in
  report (Delivery.validate_transition ~previous:(Some failed) retry);
  let failed = Delivery.fail retry error |> get in
  report (Delivery.retry failed ~max_attempts:2);
  print_s [%sexp (failed.attempt : int)];
  [%expect
    {|
    invalid_state
    invalid_state
    ok
    resource_limit
    2 |}]
;;

let%expect_test "completion and wake codecs preserve terminal and delivery distinctions" =
  List.iter
    [ Completion.Succeeded `Null; Failed error; Cancelled "cancel"; Expired ]
    ~f:(fun c ->
      let restored = Completion.of_json (Completion.to_json c) |> get in
      assert (Sexp.equal (Completion.sexp_of_t c) (Completion.sexp_of_t restored)));
  List.iter [ Completion.Request_turn; Next_turn; No_wake ] ~f:(fun wake ->
    assert (
      Completion.equal_wake
        wake
        (Completion.wake_of_json (Completion.wake_to_json wake) |> get)));
  report
    (Completion.of_json
       (Invocation.outcome_to_json
          (Pending (Job (get (Id.Job.of_string "job_pending")), `Null))));
  report
    (Completion.of_json
       (`Object [ "type", `String "expired"; "value", `String "unexpected" ]));
  report
    (Delivery.of_json
       (Agent_invocation_test.replace_field
          (Delivery.to_json (delivery ()))
          "invocation_id"
          (`String "sub_forged")));
  print_endline "terminal outcomes and wake policies round-trip";
  [%expect
    {|
    invalid_request
    invalid_request
    invalid_request
    terminal outcomes and wake policies round-trip |}]
;;
