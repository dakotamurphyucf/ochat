open! Core
open Due_support
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt

let map_payload json ~f = set json "payload" (f (field json "payload"))

let absent json name =
  match D.Json.field json ~name with
  | Absent -> true
  | Null | Value _ -> false
;;

let key number =
  P.Idempotency_key.of_string ("execution:" ^ Int.to_string number) |> protocol_ok
;;

let capture definition =
  let nominal = N.create_interval definition ~slot:0L |> protocol_ok in
  Intent.create
    ~definition
    ~control_revision:2L
    ~nominal_json:(N.to_json nominal)
    ~target:session
    ~source:(source 1L)
    ~child:None
  |> protocol_ok
;;

let replace_target definition target =
  F.create
    ~key:(F.key definition)
    ~creator:(F.creator definition)
    ~source:(F.source definition)
    ~birth_lifecycle_revision:(F.birth_lifecycle_revision definition)
    ~schedule:(F.schedule definition)
    ~target
    ~policy:(F.policy definition)
    ~created_at:(F.created_at definition)
  |> protocol_ok
;;

let%expect_test
    "one immutable input reconstructs exact bounded attempts including self target"
  =
  let definition = definition "execution" in
  let intent = capture definition in
  assert (
    P.Session_ref.equal
      (Intent.target intent)
      (I.Definition.controller (F.key definition)));
  let attempts =
    List.init Attempt.maximum_count ~f:(fun number ->
      let expected_revision = Int64.of_int (number + 7) in
      let attempt =
        Attempt.create ~intent ~number ~principal_id ~expected_revision ~key:(key number)
        |> protocol_ok
      in
      let expected =
        A.Run_admission_request.Headless.create
          ~session_id:(P.Session_ref.session_id session)
          ~generation:0
          ~expected_revision
          ~mode:Single_turn
          ~input:
            (User_submission { kind = Plain_text; text = "due fixture"; attachments = [] })
          ~key:(key number)
        |> protocol_ok
      in
      let expected_digest =
        A.Run_admission_request.request_sha256
          (A.Run_admission_request.of_headless expected)
        |> protocol_ok
      in
      [%test_eq: string] (Attempt.request_sha256 attempt) expected_digest;
      assert (Result.is_ok (Attempt.request attempt ~intent));
      let payload = field (Attempt.to_json attempt) "payload" in
      assert (absent payload "input");
      assert (absent payload "nominal");
      attempt)
  in
  let original = List.hd_exn attempts in
  let other_principal =
    P.Id.Principal.of_string "pri_fresh_authorized_reader" |> protocol_ok
  in
  let different_caller =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id:other_principal
      ~expected_revision:7L
      ~key:(key 0)
    |> protocol_ok
  in
  [%test_eq: string]
    (Attempt.request_sha256 original)
    (Attempt.request_sha256 different_caller);
  assert (not (String.equal (Attempt.digest original) (Attempt.digest different_caller)));
  List.iter [ -1; Attempt.maximum_count ] ~f:(fun number ->
    assert (
      Result.is_error
        (Attempt.create ~intent ~number ~principal_id ~expected_revision:7L ~key:(key 0))));
  let changed_digest =
    Attempt.to_json original
    |> map_payload ~f:(fun payload ->
      set payload "request_sha256" (`String (String.make 64 'f')))
    |> Attempt.of_json
    |> protocol_ok
  in
  assert (Result.is_error (Attempt.validate changed_digest ~intent));
  print_s
    [%sexp
      (List.length attempts : int)
    , ("one input; original CAS and caller retained" : string)];
  [%expect {| (8 "one input; original CAS and caller retained") |}]
;;

let%expect_test "complete content and compatible metadata survive without becoming proof" =
  let definition = definition "execution-unknown" in
  let target =
    F.target definition
    |> A.Recurrence_target.to_document
    |> document_ok
    |> D.Document.json
    |> map_payload ~f:(fun payload ->
      let input =
        append (field payload "input") "future_input" (`Number "1.00e2")
        |> fun input -> append input "padding" (`String "")
      in
      let bytes =
        D.Json.validate_and_measure ~limits:A.Recurrence_target.limits input
        |> document_ok
      in
      let input = set input "padding" (`String (String.make (262144 - bytes) 'x')) in
      set payload "input" input)
    |> D.Document.inspect ~limits:A.Recurrence_target.limits
    |> document_ok
    |> A.Recurrence_target.of_document
    |> document_ok
  in
  let definition = replace_target definition target in
  let nominal =
    N.create_interval definition ~slot:0L
    |> protocol_ok
    |> N.to_json
    |> fun json -> append json "future_nominal" (`Object [ "value", `Number "2.00e1" ])
  in
  let intent =
    Intent.create
      ~definition
      ~control_revision:2L
      ~nominal_json:nominal
      ~target:session
      ~source:(source 1L)
      ~child:None
    |> protocol_ok
  in
  let raw =
    Intent.to_json intent |> fun json -> append json "future_envelope" (`Number "3.00e1")
  in
  let restored = Intent.of_json raw |> protocol_ok in
  assert (D.Json.equal raw (Intent.to_json restored));
  assert (D.Json.equal nominal (Intent.nominal_json restored));
  assert (
    D.Json.equal
      (field (field (Intent.input_json restored) "content") "future_input")
      (`Number "1.00e2"));
  assert (Result.is_ok (Intent.validate restored ~definition ~reservation:None));
  let attempt =
    Attempt.create
      ~intent:restored
      ~number:0
      ~principal_id
      ~expected_revision:7L
      ~key:(key 0)
    |> protocol_ok
  in
  assert (Result.is_error (Attempt.validate attempt ~intent));
  let malformed =
    [ map_payload raw ~f:(fun payload -> set payload "control_revision" (`String "-1"))
    ; map_payload raw ~f:(fun payload -> set payload "mode" (`String "implicit"))
    ; map_payload raw ~f:(fun payload -> set payload "source" `Null)
    ; append raw "too_large" (`String (String.make (384 * 1024) 'x'))
    ]
  in
  List.iter malformed ~f:(fun json -> assert (Result.is_error (Intent.of_json json)));
  print_s
    [%sexp ("exact input/nominal/envelope; bounded restoration; no authority" : string)];
  [%expect {| "exact input/nominal/envelope; bounded restoration; no authority" |}]
;;
