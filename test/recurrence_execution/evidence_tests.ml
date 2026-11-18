open! Core
open Due_support
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Origin = A.Recurrence_run_origin
module Reference = A.Recurrence_run_reference

let map_payload json ~f = set json "payload" (f (field json "payload"))

(* Discover current constructor bounds, rather than duplicating the ID/source
   byte limits in the capacity oracle. No actual actor observation is issued. *)
let maximal of_string prefix character =
  let rec grow count previous =
    if count > 8192 then failwith "unexpectedly unbounded evidence field";
    match of_string (prefix ^ String.make count character) with
    | Ok value -> grow (count + 1) (Some value)
    | Error _ -> Option.value_exn previous
  in
  grow 1 None
;;

let evidence_fixture () =
  let principal = maximal P.Id.Principal.of_string "pri_" 'p' in
  let session =
    P.Session_ref.create
      ~server_id:(maximal P.Id.Server.of_string "srv_" 's')
      ~session_id:(maximal P.Id.Session.of_string "ses_" 't')
  in
  let source =
    maximal
      (fun script_id ->
         P.Run_source.create
           ~observer:{ script_id; source_sha256 = String.make 64 'a' }
           ~generation:Int.max_value
           ~installation_epoch:Int64.max_value)
      ""
      '\001'
  in
  let base = definition "evidence" in
  let target =
    A.Recurrence_target.create_existing
      ~session
      ~source
      ~configuration_revision:Int64.max_value
      ~input:
        P.Session.Message_content.{ kind = Plain_text; text = "exact"; attachments = [] }
    |> protocol_ok
  in
  let definition =
    F.create
      ~key:
        (I.Definition.create
           ~controller:session
           ~recurrence_id:(maximal P.Id.Recurrence.of_string "rec_" 'r')
           ~revision:Int64.max_value
         |> protocol_ok)
      ~creator:principal
      ~source
      ~birth_lifecycle_revision:(Lifecycle.of_int64 Int64.max_value |> protocol_ok)
      ~schedule:(F.schedule base)
      ~target
      ~policy:(F.policy base)
      ~created_at:timestamp
    |> protocol_ok
  in
  let intent =
    Intent.create
      ~definition
      ~control_revision:Int64.max_value
      ~nominal_json:(N.create_interval definition ~slot:0L |> protocol_ok |> N.to_json)
      ~target:session
      ~source
      ~child:None
    |> protocol_ok
  in
  let attempt =
    Attempt.create
      ~intent
      ~number:7
      ~principal_id:principal
      ~expected_revision:Int64.(max_value - 1L)
      ~key:(maximal P.Idempotency_key.of_string "" 'k')
    |> protocol_ok
  in
  let run_id = maximal P.Id.Run.of_string "run_" 'r' in
  let run lifecycle revision =
    P.Run.create
      ~id:run_id
      ~session
      ~principal_id:principal
      ~source
      ~mode:Single_turn
      ~lifecycle
      ~revision
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:timestamp
      ~updated_at:timestamp
    |> protocol_ok
  in
  let receipt ~kind ~key ~request_sha256 ~run_revision =
    P.Run_receipt.create
      ~run_id
      ~principal_id:principal
      ~source
      ~key
      ~request_sha256
      ~kind
      ~run_revision
      ~session_revision:Int64.max_value
      ~committed_at:timestamp
    |> protocol_ok
  in
  let admission =
    receipt
      ~kind:Admission
      ~key:(Attempt.key attempt)
      ~request_sha256:(Attempt.request_sha256 attempt)
      ~run_revision:0L
  in
  let origin =
    Origin.create ~intent ~attempt ~run:(run Admitted 0L) ~admission |> protocol_ok
  in
  let terminal terminal =
    let digest =
      P.Run.Terminal.to_json terminal
      |> Jsonaf.to_string
      |> Digestif.SHA256.digest_string
      |> Digestif.SHA256.to_hex
    in
    ( run (Terminal terminal) Int64.max_value
    , receipt
        ~kind:Terminal
        ~key:(maximal P.Idempotency_key.of_string "" 't')
        ~request_sha256:digest
        ~run_revision:Int64.max_value )
  in
  intent, attempt, origin, terminal
;;

let%expect_test
    "actual bounded Run evidence is compact; restored classification is unresolved"
  =
  let intent, attempt, origin, terminal = evidence_fixture () in
  Origin.validate origin ~intent ~attempt |> protocol_ok;
  A.Recurrence_retry_resolution.create
    ~intent
    ~attempt
    ~target_revision:0L
    ~resolved_at:timestamp
  |> protocol_ok
  |> A.Recurrence_retry_resolution.to_json
  |> D.Json.validate ~limits:A.Recurrence_retry_resolution.limits
  |> document_ok;
  let raw_origin =
    Origin.to_json origin
    |> map_payload ~f:(fun payload ->
      set
        payload
        "admission"
        (append (field payload "admission") "future_receipt" (`Number "1.00e2")))
  in
  let origin = Origin.of_json raw_origin |> protocol_ok in
  assert (D.Json.equal raw_origin (Origin.to_json origin));
  Origin.validate origin ~intent ~attempt |> protocol_ok;
  assert (
    Result.is_error
      (Origin.of_json (append raw_origin "oversized" (`String (String.make 8192 'x')))));
  let result =
    P.Run_result_reference.operation
      ~operation_id:(maximal P.Id.Operation.of_string "op_" 'o')
      ~generation:Int.max_value
      ~history_ids:
        (List.init 4096 ~f:(fun index ->
           P.History.Id.of_string ("8:evidence:" ^ Int.to_string index) |> protocol_ok))
      ~revision:Int64.max_value
    |> protocol_ok
  in
  let outcomes =
    [ P.Run.Terminal.Completed (Some result)
    ; Failed (Some Resource_limit)
    ; Cancelled
    ; Limited
    ; Interrupted
    ]
  in
  List.iter outcomes ~f:(fun outcome ->
    let run, receipt = terminal outcome in
    let reference = Reference.create ~origin ~run ~receipt |> protocol_ok in
    let raw =
      Reference.to_json reference
      |> fun json -> append json "future_reference" (`Number "2.00e1")
    in
    let restored = Reference.of_json raw |> protocol_ok in
    assert (D.Json.equal raw (Reference.to_json restored));
    Reference.validate restored ~origin ~run ~receipt |> protocol_ok;
    D.Json.validate ~limits:Reference.limits raw |> document_ok;
    assert (
      Result.is_error
        (Reference.of_json (append raw "oversized" (`String (String.make 8192 'x'))))));
  let failed, receipt = terminal (Failed (Some Resource_limit)) in
  let actual = Reference.create ~origin ~run:failed ~receipt |> protocol_ok in
  let forged_projection =
    Reference.to_json actual
    |> map_payload ~f:(fun payload ->
      set payload "outcome" (`Object [ "kind", `String "completed" ]))
    |> Reference.of_json
    |> protocol_ok
  in
  assert (
    Result.is_error (Reference.validate forged_projection ~origin ~run:failed ~receipt));
  let wrong_digest =
    P.Run_receipt.create
      ~run_id:receipt.run_id
      ~principal_id:receipt.principal_id
      ~source:receipt.source
      ~key:receipt.key
      ~request_sha256:(String.make 64 'f')
      ~kind:Terminal
      ~run_revision:receipt.run_revision
      ~session_revision:receipt.session_revision
      ~committed_at:receipt.committed_at
    |> protocol_ok
  in
  assert (Result.is_error (Reference.create ~origin ~run:failed ~receipt:wrong_digest));
  let wrong_kind =
    Origin.to_json origin
    |> map_payload ~f:(fun payload ->
      set
        payload
        "admission"
        (set (field payload "admission") "kind" (`String "terminal")))
  in
  assert (Result.is_error (Origin.of_json wrong_kind));
  print_s
    [%sexp
      ("maximal escaped source/IDs; 4096 result IDs stay in actual Run; decoded tag \
        grants nothing"
       : string)];
  [%expect
    {| "maximal escaped source/IDs; 4096 result IDs stay in actual Run; decoded tag grants nothing" |}]
;;
