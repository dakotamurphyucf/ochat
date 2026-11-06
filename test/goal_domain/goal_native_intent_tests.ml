open! Core
module P = Agent_protocol
module Intent = P.Goal_native_resume_intent
module Request = Goal_resume_request_tests

let ok = Request.ok

let fields = function
  | `Object fields -> fields
  | _ -> failwith "expected object"
;;

let replace value name field =
  `Object (List.Assoc.add (fields value) ~equal:String.equal name field)
;;

let timestamp text = P.Timestamp.of_string text |> ok

let%expect_test "native resume phases retain exact identity and reserve terminal growth" =
  let request = Request.request () |> ok in
  let principal_id = P.Id.Principal.of_string "pri_native_goal" |> ok in
  let staged = Intent.create ~request ~principal_id |> ok in
  let accepted_at = timestamp "2026-11-01T00:00:00Z" in
  let claimed_at = timestamp "2026-10-31T23:59:59Z" in
  let accepted = Intent.accept staged ~now:accepted_at |> ok in
  let claimed = Intent.claim accepted ~now:claimed_at |> ok in
  Intent.validate_update staged ~incoming:accepted |> ok;
  Intent.validate_update accepted ~incoming:claimed |> ok;
  (match Intent.phase claimed with
   | Dispatch_claimed value ->
     assert (P.Timestamp.equal accepted_at value.accepted_at);
     assert (P.Timestamp.equal claimed_at value.claimed_at)
   | _ -> assert false);
  let altered_acceptance =
    let wire = Intent.to_json claimed in
    let phase = Jsonaf.member_exn "phase" wire in
    replace wire "phase" (replace phase "accepted_at" (P.Timestamp.to_json claimed_at))
    |> Intent.of_json
    |> ok
  in
  assert (Result.is_error (Intent.validate_update accepted ~incoming:altered_acceptance));
  let huge = String.make 100_000 'x' in
  let primary = P.Error.create Permission_denied ~message:huge ~retryable:false () in
  let rejected = Intent.reject claimed primary |> ok in
  let discarded = Intent.discard accepted ~reason:huge |> ok in
  List.iter [ staged; accepted; claimed; discarded; rejected ] ~f:(fun value ->
    assert (Intent.equal value (Intent.of_json (Intent.to_json value) |> ok));
    assert (String.length (Jsonaf.to_string (Intent.to_json value)) <= 8192));
  (match Intent.phase rejected with
   | Rejected error ->
     [%test_eq: P.Error.code] Permission_denied error.code;
     assert (not error.retryable);
     assert (String.length error.message < String.length primary.message)
   | _ -> assert false);
  assert (String.equal primary.message huge);
  List.iter [ accepted; claimed; discarded; rejected ] ~f:(fun value ->
    assert (Result.is_error (Intent.validate_update value ~incoming:staged)));
  assert (Result.is_error (Intent.claim staged ~now:claimed_at));
  assert (Result.is_error (Intent.discard claimed ~reason:"late"));
  let other =
    Intent.create
      ~request:
        (Request.request ~generation:1 ~source:(Request.make_source ~generation:1 ()) ()
         |> ok)
      ~principal_id
    |> ok
  in
  assert (Result.is_error (Intent.validate_update staged ~incoming:other));
  List.iter
    [ replace (Intent.to_json staged) "phase" `Null
    ; replace (Intent.to_json staged) "phase" (`Object [ "kind", `String "future" ])
    ; replace (Intent.to_json staged) "future_permission" `True
    ]
    ~f:(fun wire -> assert (Result.is_error (Intent.of_json wire)));
  print_endline
    "exact identity and accepted_at; wallclock regression allowed; bounded terminal \
     phases; no rearm";
  [%expect
    {| exact identity and accepted_at; wallclock regression allowed; bounded terminal phases; no rearm |}]
;;

let document_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let%expect_test
    "native child requires semantics; historical absence and typed edit fences roundtrip"
  =
  let request = Request.request () |> ok in
  let principal_id = P.Id.Principal.of_string "pri_native_goal" |> ok in
  let intent = Intent.create ~request ~principal_id |> ok in
  let context : P.Invocation.context =
    { id = P.Id.Invocation.of_string "inv_native_goal_codec" |> ok
    ; session_id = P.Goal_resume_request.session_id request
    ; generation = 0
    ; origin = Script
    ; provider_call_id = None
    ; call_entry_id = None
    ; parent_invocation = None
    ; parent_job = None
    ; tool_name = "goal_resume"
    ; implementation_revision = String.make 64 'a'
    ; capability_fingerprint = String.make 64 'b'
    ; input = `Object []
    ; created_at = timestamp "2026-11-01T00:00:00Z"
    ; deadline = None
    }
  in
  let plain = P.Invocation.create context |> ok in
  let plain_wire = P.Invocation.to_json plain in
  assert (
    not (List.Assoc.mem (fields plain_wire) "native_goal_action" ~equal:String.equal));
  assert (
    Jsonaf.exactly_equal
      plain_wire
      (P.Invocation.of_json plain_wire |> ok |> P.Invocation.to_json));
  let resolved =
    P.Invocation.dispatch plain
    |> ok
    |> fun dispatched ->
    P.Invocation.resolve
      dispatched
      ~session_id:context.session_id
      ~generation:0
      (Complete `Null)
    |> ok
  in
  let staged =
    P.Invocation.record_native_goal_action
      resolved
      (P.Goal_native_action_intent.Resume intent)
    |> ok
  in
  assert (
    P.Invocation.equal staged (P.Invocation.of_json (P.Invocation.to_json staged) |> ok));
  assert (
    Result.is_error
      (P.Invocation.of_json
         (replace (P.Invocation.to_json staged) "native_goal_action" `Null)));
  (* Local carrier law only: these decoded values supply no native admission.
     A validated phase change may retain the same successful outcome; it cannot
     rewrite that outcome, context, child identity or history. *)
  let accepted_intent =
    Intent.accept intent ~now:(timestamp "2026-11-01T00:00:00Z") |> ok
  in
  let accepted =
    P.Invocation.record_native_goal_action
      staged
      (P.Goal_native_action_intent.Resume accepted_intent)
    |> ok
  in
  let claimed_intent =
    Intent.claim accepted_intent ~now:(timestamp "2026-11-01T00:00:01Z") |> ok
  in
  let claimed =
    P.Invocation.record_native_goal_action
      accepted
      (P.Goal_native_action_intent.Resume claimed_intent)
    |> ok
  in
  let rejected_intent =
    Intent.reject claimed_intent (P.Error.invalid_request "current command denied") |> ok
  in
  let rejected =
    P.Invocation.record_native_goal_action
      claimed
      (P.Goal_native_action_intent.Resume rejected_intent)
    |> ok
  in
  let discarded_intent =
    Intent.discard accepted_intent ~reason:"callback retired" |> ok
  in
  let discarded =
    P.Invocation.record_native_goal_action
      accepted
      (P.Goal_native_action_intent.Resume discarded_intent)
    |> ok
  in
  List.iter
    [ staged, accepted; accepted, claimed; claimed, rejected; accepted, discarded ]
    ~f:(fun (previous, incoming) ->
      P.Invocation.validate_transition ~previous:(Some previous) incoming |> ok;
      match incoming.P.Invocation.status with
      | Resolved (Complete `Null) -> ()
      | _ -> failwith "native phase update replaced successful output");
  let foreign_intent =
    Intent.create
      ~request
      ~principal_id:(P.Id.Principal.of_string "pri_other_native_goal" |> ok)
    |> ok
    |> fun intent -> Intent.accept intent ~now:(timestamp "2026-11-01T00:00:00Z") |> ok
  in
  let claimed_wire = P.Invocation.to_json claimed in
  let altered_status =
    Jsonaf.member_exn "status" claimed_wire
    |> fun status ->
    replace
      status
      "outcome"
      (P.Invocation.outcome_to_json (Complete (`String "replaced output")))
  in
  let altered_context =
    Jsonaf.member_exn "context" claimed_wire
    |> fun value -> replace value "id" (`String "inv_other_native_goal")
  in
  let altered name value =
    replace claimed_wire name value |> P.Invocation.of_json |> ok
  in
  List.iter
    [ altered "status" altered_status
    ; altered "context" altered_context
    ; resolved
    ; altered
        "native_goal_action"
        (P.Goal_native_action_intent.to_json (Resume foreign_intent))
    ; staged
    ; accepted
    ]
    ~f:(fun incoming ->
      assert (
        Result.is_error
          (P.Invocation.validate_transition ~previous:(Some accepted) incoming)));
  let limits = Document_schema.Limits.default in
  let delta =
    Agent_session.Session_delta_document.create
      (Invocation_changed staged)
      ~limits
      ~state_document:(fun _ -> failwith "this test has no Created leaf")
    |> document_ok
    |> Agent_session.Session_delta_document.document
  in
  assert (
    List.mem
      (Document_schema.Document.required_semantics delta)
      Intent.persistence_semantics
      ~equal:String.equal);
  let altered markers =
    replace (Document_schema.Document.json delta) "required_semantics" markers
    |> Document_schema.Document.inspect ~limits
    |> document_ok
  in
  List.iter
    [ altered (`Array []); altered (`Array [ `String "goal.future-resume.v999" ]) ]
    ~f:(fun document ->
      assert (
        Result.is_error (Agent_session.Session_delta_document.decode ~limits document)));
  let edit =
    P.Goal_edit.create
      ~generation:0
      ~source:Request.source
      ~goal_id:(P.Goal_resume_request.goal_id request)
      ~expected_goal_revision:0L
      ~expected_catalog_revision:0L
      ~key:(P.Idempotency_key.of_string "native-edit" |> ok)
      ~mutation:(Record_progress (P.Goal.Progress.of_string "actual progress" |> ok))
    |> ok
  in
  let wire = P.Goal_edit.to_json edit in
  assert (Jsonaf.exactly_equal wire (P.Goal_edit.of_json wire |> ok |> P.Goal_edit.to_json));
  List.iter
    [ replace wire "generation" (`Number "1")
    ; replace wire "source" `Null
    ; replace wire "authority" `True
    ]
    ~f:(fun wire -> assert (Result.is_error (P.Goal_edit.of_json wire)));
  print_endline
    "absence unchanged; new child requires understood semantics; null and unfrozen edit \
     fences reject";
  [%expect
    {| absence unchanged; new child requires understood semantics; null and unfrozen edit fences reject |}]
;;
