open Core
open Agent_protocol

let get = function
  | Ok value -> value
  | Error (error : Error.t) -> failwith error.message
;;

let report result =
  print_endline
    (match result with
     | Ok _ -> "ok"
     | Error (error : Error.t) -> Error.code_to_string error.code)
;;

let context () : Invocation.context =
  { id = get (Id.Invocation.of_string "inv_example")
  ; session_id = get (Id.Session.of_string "ses_parent")
  ; generation = 3
  ; origin = Model
  ; provider_call_id = Some "provider-call-1"
  ; call_entry_id = None
  ; parent_invocation = None
  ; parent_job = None
  ; tool_name = "watch_response"
  ; implementation_revision = "revision-1"
  ; capability_fingerprint = "capabilities-1"
  ; input = `Object [ "child", `String "ses_child" ]
  ; created_at = get (Timestamp.of_string "2026-09-08T12:00:00Z")
  ; deadline = Some (get (Timestamp.of_string "2026-09-08T12:01:00Z"))
  }
;;

let resolve invocation outcome =
  Invocation.resolve
    invocation
    ~session_id:invocation.context.session_id
    ~generation:invocation.context.generation
    outcome
;;

let replace_field json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "expected object fixture"
;;

let%test_unit
    "deferred observation intent survives outcomes and permits one handling attempt"
  =
  let module I = Invocation in
  let observer : I.observer =
    { script_id = "moderator"; source_sha256 = String.make 64 'a' }
  in
  let ctx =
    { (context ()) with
      origin = Moderator
    ; provider_call_id = None
    ; parent_invocation = Some (get (Id.Invocation.of_string "inv_parent"))
    }
  in
  let admitted = get (I.create ~observer ctx) in
  let roundtrip invocation =
    assert (I.equal invocation (get (I.of_json (I.to_json invocation))));
    assert (I.equal invocation (I.t_of_sexp (I.sexp_of_t invocation)))
  in
  let transition previous next =
    get (I.validate_transition ~previous:(Some previous) next);
    roundtrip next
  in
  let rejected value = assert (Result.is_error value) in
  roundtrip admitted;
  rejected (I.create ~observer (context ()));
  rejected (I.create ~observer:{ observer with source_sha256 = "bad" } ctx);
  rejected (I.claim_observation admitted);
  rejected (I.complete_observation admitted);
  let dispatched = get (I.dispatch admitted) in
  transition admitted dispatched;
  rejected (I.claim_observation dispatched);
  List.iter
    [ I.Complete (`String "saved")
    ; I.Fail { code = "denied"; message = "denied"; retryable = false; details = `Null }
    ; I.Cancelled "stop"
    ]
    ~f:(fun outcome ->
      let resolved = get (resolve dispatched outcome) in
      transition dispatched resolved;
      rejected (I.complete_observation resolved);
      let claimed = get (I.claim_observation resolved) in
      transition resolved claimed;
      rejected (I.claim_observation claimed);
      let completed = get (I.complete_observation claimed) in
      transition claimed completed;
      rejected (I.claim_observation completed);
      rejected (I.fail_observation completed ~reason:"late failure");
      assert (I.equal_status completed.status resolved.status);
      List.iter [ resolved; claimed ] ~f:(fun previous ->
        let failed = get (I.fail_observation previous ~reason:"interrupted") in
        transition previous failed;
        assert (I.equal_status failed.status resolved.status);
        assert (I.equal failed (get (I.fail_observation failed ~reason:"interrupted")));
        rejected (I.fail_observation failed ~reason:"replacement");
        rejected (I.claim_observation failed));
      let publish = get (I.publish claimed) in
      transition claimed publish;
      transition publish (get (I.complete_observation publish));
      let forge json = get (I.of_json json) in
      let changed_owner =
        match I.to_json claimed with
        | `Object fields ->
          let observation =
            List.Assoc.find_exn fields ~equal:String.equal "observation"
          in
          replace_field
            (I.to_json claimed)
            "observation"
            (replace_field observation "script_id" (`String "other"))
          |> forge
        | _ -> assert false
      in
      rejected (I.validate_transition ~previous:(Some resolved) changed_owner);
      rejected (I.validate_transition ~previous:(Some completed) claimed);
      rejected (I.validate_transition ~previous:(Some resolved) completed);
      let without =
        get (I.create ctx)
        |> I.dispatch
        |> get
        |> fun value -> get (resolve value outcome)
      in
      rejected (I.validate_transition ~previous:(Some without) claimed);
      rejected (I.validate_transition ~previous:(Some claimed) without));
  let legacy = get (I.create ctx) in
  roundtrip legacy;
  let json = I.to_json admitted in
  rejected (I.of_json (replace_field json "schema_version" (`Number "4")));
  rejected (I.of_json (replace_field json "observation" (`Object [])))
;;

let%expect_test
    "observation follow-up intent survives acknowledgement until durably applied"
  =
  let module I = Invocation in
  let observer : I.observer =
    { script_id = "moderator"; source_sha256 = String.make 64 'a' }
  in
  let ctx =
    { (context ()) with
      origin = Moderator
    ; provider_call_id = None
    ; parent_invocation = Some (get (Id.Invocation.of_string "inv_parent"))
    }
  in
  let claimed =
    I.create ~observer ctx
    |> get
    |> I.dispatch
    |> get
    |> fun invocation ->
    resolve invocation (Complete (`String "native result"))
    |> get
    |> I.claim_observation
    |> get
  in
  let requests : I.follow_up =
    { request_turn = true; request_compaction = true; end_session = Some "finished" }
  in
  let pending = I.complete_observation ~follow_up:requests claimed |> get in
  get (I.validate_transition ~previous:(Some claimed) pending);
  let restored = I.of_json (I.to_json pending) |> get in
  assert (I.equal pending restored);
  assert (I.equal pending (I.t_of_sexp (I.sexp_of_t pending)));
  let applied = I.apply_observation_follow_up restored |> get in
  get (I.validate_transition ~previous:(Some restored) applied);
  assert (I.equal applied (I.of_json (I.to_json applied) |> get));
  assert (I.equal applied (I.apply_observation_follow_up applied |> get));
  assert (I.equal_status pending.status applied.status);
  print_s
    [%sexp
      { pending =
          ((Option.value_exn pending.observation).follow_up : I.follow_up_status option)
      ; applied =
          ((Option.value_exn applied.observation).follow_up : I.follow_up_status option)
      }];
  let reject label result =
    match result with
    | Ok _ -> failwith (label ^ " unexpectedly succeeded")
    | Error (error : Error.t) ->
      print_endline (label ^ ": " ^ Error.code_to_string error.code)
  in
  let no_actions = I.complete_observation claimed |> get in
  reject "apply before acknowledgement" (I.apply_observation_follow_up claimed);
  reject
    "empty requests"
    (I.complete_observation
       ~follow_up:{ request_turn = false; request_compaction = false; end_session = None }
       claimed);
  reject
    "oversized stop reason"
    (I.complete_observation
       ~follow_up:{ requests with end_session = Some (String.make 1025 'x') }
       claimed);
  reject
    "late intent attachment"
    (I.validate_transition ~previous:(Some no_actions) pending);
  reject "skip pending intent" (I.validate_transition ~previous:(Some claimed) applied);
  reject "rearm applied intent" (I.validate_transition ~previous:(Some applied) pending);
  reject
    "discard pending intent"
    (I.validate_transition ~previous:(Some pending) no_actions);
  let different =
    I.complete_observation ~follow_up:{ requests with request_turn = false } claimed
    |> get
  in
  reject "replace actions" (I.validate_transition ~previous:(Some pending) different);
  reject
    "hide new fields in codec 5"
    (I.of_json (replace_field (I.to_json pending) "schema_version" (`Number "5")));
  reject
    "codec 6 without intent"
    (I.of_json (replace_field (I.to_json no_actions) "schema_version" (`Number "6")));
  assert (I.equal no_actions (I.of_json (I.to_json no_actions) |> get));
  [%expect
    {|
    ((pending
      ((Pending_follow_up
        ((request_turn true) (request_compaction true) (end_session (finished))))))
     (applied
      ((Applied_follow_up
        ((request_turn true) (request_compaction true) (end_session (finished)))))))
    apply before acknowledgement: invalid_state
    empty requests: invalid_request
    oversized stop reason: invalid_request
    late intent attachment: conflict
    skip pending intent: conflict
    rearm applied intent: conflict
    discard pending intent: conflict
    replace actions: conflict
    hide new fields in codec 5: invalid_request
    codec 6 without intent: invalid_request
    |}]
;;

let%expect_test "recorded pending outcome survives restart without a second resolution" =
  let admitted = get (Invocation.create (context ())) in
  report (Invocation.publish admitted);
  let dispatched = get (Invocation.dispatch admitted) in
  let outcome =
    Invocation.Pending
      (Subscription (get (Id.Subscription.of_string "sub_watch")), `String "watching")
  in
  let resolved = get (resolve dispatched outcome) in
  let restored = get (Invocation.of_json (Invocation.to_json resolved)) in
  report (resolve restored outcome);
  let published = get (Invocation.publish restored) in
  let repeated = get (Invocation.publish published) in
  print_s
    [%sexp
      (String.equal
         (Jsonaf.to_string (Invocation.to_json published))
         (Jsonaf.to_string (Invocation.to_json repeated))
       : bool)];
  report (Invocation.cancel published ~reason:"late cancel");
  [%expect
    {|
    invalid_state
    already_resolved
    true
    already_resolved |}]
;;

let%expect_test "resolution checks owner and generation before changing the outcome" =
  let invocation = get (Invocation.create (context ())) |> Invocation.dispatch |> get in
  report
    (Invocation.resolve
       invocation
       ~session_id:(get (Id.Session.of_string "ses_foreign"))
       ~generation:3
       (Complete `Null));
  report
    (Invocation.resolve
       invocation
       ~session_id:invocation.context.session_id
       ~generation:4
       (Complete `Null));
  report (resolve invocation (Complete `Null));
  [%expect
    {|
    permission_denied
    conflict
    ok |}]
;;

let%expect_test "script origins cannot fabricate provider call history" =
  let original = context () in
  report (Invocation.create { original with origin = Script });
  report (Invocation.create { original with provider_call_id = None });
  report (Invocation.create { original with origin = Script; provider_call_id = None });
  report (Invocation.create { original with parent_invocation = Some original.id });
  report (Invocation.create { original with generation = -1 });
  report
    (Invocation.create
       { original with
         deadline = Some (get (Timestamp.of_string "2026-09-07T12:00:00Z"))
       });
  [%expect
    {|
    invalid_request
    invalid_request
    ok
    invalid_request
    invalid_request
    invalid_request |}]
;;

let%expect_test
    "outcomes preserve typed work and errors independently of success payloads"
  =
  List.iter
    [ Invocation.Complete (`Object [ "count", `Number "2" ])
    ; Pending (Job (get (Id.Job.of_string "job_fixture")), `Null)
    ; Pending (Subscription (get (Id.Subscription.of_string "sub_fixture")), `True)
    ; Fail
        { code = "output_schema"
        ; message = "invalid result"
        ; retryable = false
        ; details = `Null
        }
    ; Cancelled "deadline"
    ]
    ~f:(fun outcome ->
      let encoded = Invocation.outcome_to_json outcome in
      let restored = get (Invocation.outcome_of_json encoded) in
      assert (
        String.equal
          (Jsonaf.to_string encoded)
          (Jsonaf.to_string (Invocation.outcome_to_json restored))));
  print_endline "all outcomes round-trip";
  let forged =
    `Object
      [ "type", `String "pending"
      ; "work", `Object [ "type", `String "job"; "id", `String "sub_fixture" ]
      ; "acknowledgement", `Null
      ]
  in
  report (Invocation.outcome_of_json forged);
  report
    (Invocation.outcome_of_json
       (`Object [ "type", `String "complete"; "value", `Null; "work", `String "job_fake" ]));
  [%expect
    {|
    all outcomes round-trip
    invalid_request
    invalid_request |}]
;;

let%expect_test "incompatible and malformed snapshots fail instead of losing state" =
  let invocation = get (Invocation.create (context ())) in
  let encoded = Invocation.to_json invocation in
  report (Invocation.of_json (replace_field encoded "schema_version" (`Number "9")));
  report
    (Invocation.of_json
       (replace_field encoded "status" (`Object [ "type", `String "resolved" ])));
  report
    (Invocation.of_json
       (replace_field
          encoded
          "status"
          (`Object
              [ "type", `String "admitted"
              ; "outcome", Invocation.outcome_to_json (Complete `Null)
              ])));
  (match encoded with
   | `Object fields ->
     report (Invocation.of_json (`Object (("schema_version", `Number "1") :: fields)))
   | _ -> assert false);
  [%expect
    {|
    incompatible_protocol
    invalid_request
    invalid_request
    invalid_request |}]
;;

let%expect_test "deep and invalid JSON is rejected before serialization or execution" =
  let original = context () in
  let deep =
    List.fold (List.init 10000 ~f:Fn.id) ~init:`Null ~f:(fun json _ -> `Array [ json ])
  in
  report (Invocation.create { original with input = deep });
  report (Invocation.create { original with input = `Number "NaN" });
  report (Invocation.create { original with input = `Number "0x20" });
  report (Invocation.create { original with input = `Number "1e9999" });
  report (Invocation.create { original with input = `Object [ "x", `Null; "x", `True ] });
  report
    (Invocation.create
       { original with input = `String (String.make (8 * 1024 * 1024) 'x') });
  [%expect
    {|
    invalid_request
    invalid_request
    invalid_request
    invalid_request
    invalid_request
    invalid_request |}]
;;

let%expect_test "record envelope preserves maximum-depth and large admitted payloads" =
  let original = context () in
  let input =
    List.fold (List.init 127 ~f:Fn.id) ~init:`Null ~f:(fun json _ -> `Array [ json ])
  in
  let invocation = get (Invocation.create { original with input }) in
  report (Invocation.of_json (Invocation.to_json invocation));
  let input = `String (String.make (1024 * 1024) 'x') in
  let invocation =
    get (Invocation.create { original with input }) |> Invocation.dispatch |> get
  in
  let invocation = get (resolve invocation (Complete input)) in
  report (Invocation.of_json (Invocation.to_json invocation));
  [%expect
    {|
    ok
    ok |}]
;;

let%expect_test "host cancellation before dispatch remains publishable after restore" =
  let invocation = get (Invocation.create (context ())) in
  let cancelled = get (Invocation.cancel invocation ~reason:"parent stopped") in
  let restored = get (Invocation.of_json (Invocation.to_json cancelled)) in
  report (Invocation.dispatch restored);
  report (Invocation.publish restored);
  [%expect
    {|
    invalid_state
    ok |}]
;;

let%expect_test "validation also checks typed IDs restored through sexp snapshots" =
  let original = context () in
  let forged = Id.Invocation.t_of_sexp (Sexp.Atom "job_forged") in
  report (Invocation.create { original with id = forged });
  let invocation = get (Invocation.create original) |> Invocation.dispatch |> get in
  let forged_job = Id.Job.t_of_sexp (Sexp.Atom "sub_forged") in
  report (resolve invocation (Pending (Job forged_job, `Null)));
  let serialized = Sexp.to_string (Invocation.sexp_of_t invocation) in
  let corrupted =
    String.substr_replace_all serialized ~pattern:"inv_example" ~with_:"job_forged"
  in
  report (Invocation.validate (Invocation.t_of_sexp (Sexp.of_string corrupted)));
  [%expect
    {|
    invalid_request
    invalid_request
    invalid_request |}]
;;

let%expect_test
    "canonical occurrence bindings survive codecs and make publication idempotent"
  =
  let call_entry_id = get (History.Id.of_string "4:test:1") in
  let output_entry_id = get (History.Id.of_string "4:test:2") in
  let original = context () in
  let admitted =
    get (Invocation.create { original with call_entry_id = Some call_entry_id })
  in
  let restored = get (Invocation.of_json (Invocation.to_json admitted)) in
  assert (Sexp.equal (Invocation.sexp_of_t admitted) (Invocation.sexp_of_t restored));
  let dispatched = get (Invocation.dispatch restored) in
  let resolved = get (resolve dispatched (Complete (`String "done"))) in
  report (Invocation.publish resolved);
  let published = get (Invocation.publish_with_history resolved ~output_entry_id) in
  report (Invocation.validate_transition ~previous:(Some resolved) published);
  let restored = get (Invocation.of_json (Invocation.to_json published)) in
  let repeated = get (Invocation.publish_with_history restored ~output_entry_id) in
  assert (Sexp.equal (Invocation.sexp_of_t repeated) (Invocation.sexp_of_t published));
  let another_output = get (History.Id.of_string "4:test:3") in
  report (Invocation.publish_with_history restored ~output_entry_id:another_output);
  let other =
    get (Invocation.publish_with_history resolved ~output_entry_id:another_output)
  in
  report (Invocation.validate_transition ~previous:(Some published) other);
  let other_context =
    get (Invocation.create { original with call_entry_id = Some output_entry_id })
  in
  report
    (Invocation.validate_transition
       ~previous:(Some admitted)
       (get (Invocation.dispatch other_context)));
  [%expect
    {|
    invalid_state
    ok
    conflict
    conflict
    conflict |}]
;;

let%expect_test "legacy records remain unbound and v2 cannot lose its occurrence receipts"
  =
  let original = context () in
  let legacy = get (Invocation.create original) in
  let json = Invocation.to_json legacy in
  assert (not (String.is_substring (Jsonaf.to_string json) ~substring:"entry_id"));
  let sexp = Invocation.sexp_of_t legacy in
  assert (not (String.is_substring (Sexp.to_string sexp) ~substring:"entry_id"));
  report (Invocation.validate (Invocation.t_of_sexp sexp));
  let call_entry_id = get (History.Id.of_string "4:test:1") in
  report (Invocation.publish_with_history legacy ~output_entry_id:call_entry_id);
  report
    (Invocation.create
       { original with
         origin = Script
       ; provider_call_id = None
       ; call_entry_id = Some call_entry_id
       });
  let bound =
    get (Invocation.create { original with call_entry_id = Some call_entry_id })
  in
  let bound_json = Invocation.to_json bound in
  report (Invocation.of_json (replace_field bound_json "schema_version" (`Number "1")));
  report (Invocation.of_json (replace_field json "schema_version" (`Number "2")));
  report
    (Invocation.of_json
       (replace_field bound_json "output_entry_id" (History.Id.to_json call_entry_id)));
  report
    (Invocation.of_json
       (replace_field
          bound_json
          "status"
          (`Object
              [ "type", `String "published"
              ; "outcome", Invocation.outcome_to_json (Complete `Null)
              ])));
  [%expect
    {|
    ok
    invalid_state
    invalid_request
    invalid_request
    invalid_request
    invalid_request
    invalid_request |}]
;;

let%test_unit "routing provenance has a closed v3 codec and immutable admission binding" =
  let module I = Invocation in
  let fp text = I.{ sha256 = String.make 64 'a'; byte_length = String.length text } in
  let original =
    { (context ()) with call_entry_id = Some (get (History.Id.of_string "4:test:1")) }
  in
  let routing =
    I.
      { kind = Function
      ; original_name = "alias"
      ; original_payload = fp "{}"
      ; final_payload = fp "null"
      ; canonical_payload = Some (fp "[redacted]")
      ; preparation = Passed
      }
  in
  let admitted = I.create ~routing original |> get in
  let json = I.to_json admitted in
  assert (
    Poly.equal
      (Json_codec.fields json
       |> get
       |> fun fields -> Json_codec.required fields "schema_version" |> get)
      (`Number "3"));
  let decoded = I.of_json json |> get in
  assert (Sexp.equal (I.sexp_of_t admitted) (I.sexp_of_t decoded));
  assert (
    Sexp.equal (I.sexp_of_t admitted) (I.sexp_of_t (I.t_of_sexp (I.sexp_of_t admitted))));
  assert (Result.is_error (I.of_json (replace_field json "schema_version" (`Number "2"))));
  assert (Result.is_error (I.of_json (replace_field json "routing" `Null)));
  let without =
    match json with
    | `Object fields -> `Object (List.Assoc.remove fields ~equal:String.equal "routing")
    | _ -> assert false
  in
  assert (Result.is_error (I.of_json without));
  let changed =
    I.create ~routing:{ routing with original_name = "another" } original
    |> get
    |> I.dispatch
    |> get
  in
  assert (Result.is_error (I.validate_transition ~previous:(Some admitted) changed));
  let removed = I.create original |> get |> I.dispatch |> get in
  assert (Result.is_error (I.validate_transition ~previous:(Some admitted) removed));
  let script =
    I.create
      ~routing:{ routing with canonical_payload = None }
      { original with origin = Script; provider_call_id = None; call_entry_id = None }
    |> get
  in
  assert (
    Sexp.equal (I.sexp_of_t script) (I.sexp_of_t (I.of_json (I.to_json script) |> get)));
  let resolved =
    I.dispatch admitted |> get |> fun inv -> resolve inv (Complete `Null) |> get
  in
  let published =
    I.publish_with_history
      resolved
      ~output_entry_id:(get (History.Id.of_string "4:test:2"))
    |> get
  in
  assert (Poly.equal published.routing (Some routing));
  assert (
    Sexp.equal
      (I.sexp_of_t published)
      (I.sexp_of_t (I.of_json (I.to_json published) |> get)))
;;

let%test_unit "discarded publication preserves outcomes and has an immutable v4 receipt" =
  let inv =
    Invocation.create { (context ()) with origin = Model; provider_call_id = Some "call" }
    |> get
  in
  assert (Result.is_error (Invocation.discard_publication inv ~reason:"removed"));
  let resolved =
    Invocation.dispatch inv
    |> get
    |> fun inv -> resolve inv (Complete (`String "retained")) |> get
  in
  let discarded = Invocation.discard_publication resolved ~reason:"removed" |> get in
  Invocation.validate_transition ~previous:(Some resolved) discarded |> get;
  assert (
    Sexp.equal
      (Invocation.sexp_of_status resolved.status)
      (Invocation.sexp_of_status discarded.status));
  let encoded = Invocation.to_json discarded in
  assert (
    Poly.equal
      (get (Json_codec.required (get (Json_codec.fields encoded)) "schema_version"))
      (`Number "4"));
  let restored = Invocation.of_json encoded |> get in
  assert (Sexp.equal (Invocation.sexp_of_t discarded) (Invocation.sexp_of_t restored));
  assert (
    Result.is_error
      (Invocation.of_json (replace_field encoded "schema_version" (`Number "3"))));
  assert (Result.is_error (Invocation.publish discarded));
  assert (
    Result.is_error
      (Invocation.publish_with_history
         discarded
         ~output_entry_id:(get (History.Id.of_string "4:test:5"))));
  assert (Result.is_error (Invocation.discard_publication discarded ~reason:"different"));
  assert (
    Result.is_error (Invocation.validate_transition ~previous:(Some discarded) resolved));
  assert (
    Sexp.equal
      (Invocation.sexp_of_t discarded)
      (Invocation.sexp_of_t
         (Invocation.discard_publication discarded ~reason:"removed" |> get)))
;;

let%test_unit "routing rejects malformed fingerprints and successful denied preparation" =
  let module I = Invocation in
  let fp = I.{ sha256 = String.make 64 'a'; byte_length = 4 } in
  let original =
    { (context ()) with call_entry_id = Some (get (History.Id.of_string "4:test:1")) }
  in
  let routing =
    I.
      { kind = Function
      ; original_name = original.tool_name
      ; original_payload = fp
      ; final_payload = fp
      ; canonical_payload = Some fp
      ; preparation = Invalid_input
      }
  in
  let make routing = I.create ~routing original in
  List.iter
    [ { fp with sha256 = "short" }
    ; { fp with sha256 = String.make 64 'G' }
    ; { fp with byte_length = -1 }
    ]
    ~f:(fun bad ->
      assert (Result.is_error (make { routing with original_payload = bad })));
  assert (Result.is_error (make { routing with original_name = "redirected" }));
  assert (
    Result.is_error (make { routing with final_payload = { fp with byte_length = 5 } }));
  assert (Result.is_error (make { routing with canonical_payload = None }));
  let inv = make routing |> get |> I.dispatch |> get in
  assert (Result.is_error (resolve inv (Complete `Null)));
  assert (
    Result.is_error
      (resolve inv (Pending (Job (get (Id.Job.of_string "job_example")), `Null))));
  ignore
    (resolve
       inv
       (Fail
          { code = "invocation.invalid_input"
          ; message = "rejected"
          ; retryable = false
          ; details = `Null
          })
     |> get);
  ignore (I.cancel inv ~reason:"cancelled" |> get);
  let stopped_after_rewrite =
    make
      { routing with
        preparation = Session_ended
      ; original_name = "alias"
      ; final_payload = { fp with byte_length = 5 }
      }
    |> get
    |> I.dispatch
    |> get
  in
  assert (Result.is_error (resolve stopped_after_rewrite (Complete `Null)));
  let stopped_after_rewrite = I.cancel stopped_after_rewrite ~reason:"stopped" |> get in
  let restored = I.of_json (I.to_json stopped_after_rewrite) |> get in
  assert (Sexp.equal (I.sexp_of_t stopped_after_rewrite) (I.sexp_of_t restored));
  List.iter [ I.Pre_tool_rejected; Pre_tool_failed; Session_ended ] ~f:(fun preparation ->
    let inv = make { routing with preparation } |> get |> I.dispatch |> get in
    assert (Result.is_error (resolve inv (Complete `Null)));
    let cancelled = I.cancel inv ~reason:"cancelled" |> get in
    let restored = I.of_json (I.to_json cancelled) |> get in
    assert (Sexp.equal (I.sexp_of_t cancelled) (I.sexp_of_t restored)))
;;
