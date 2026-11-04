open! Core
open Due_support
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Execution = A.Recurrence_execution

let map_payload json ~f = set json "payload" (f (field json "payload"))

let map_frontier json ~f =
  map_payload json ~f:(fun payload ->
    set payload "frontier" (f (field payload "frontier")))
;;

let decode json =
  D.Document.inspect ~limits:D.Limits.default json |> Result.bind ~f:E.of_document
;;

let original_nominal entry =
  let document = H.to_document (frontier entry) |> document_ok |> D.Document.json in
  match field (field document "payload") "ready" with
  | `Array [ nominal ] -> nominal
  | _ -> failwith "expected one exact Ready"
;;

let capture_intent definition entry nominal_json =
  Intent.create
    ~definition
    ~control_revision:(E.control_revision entry)
    ~nominal_json
    ~target:session
    ~source:(source 1L)
    ~child:None
  |> protocol_ok
;;

let request intent key =
  Attempt.create
    ~intent
    ~number:0
    ~principal_id
    ~expected_revision:7L
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let%expect_test
    "initial frontier record preserves claims, full custody and historical binding"
  =
  let definition = definition "frontier-execution" in
  let selected, _, changed =
    step (enabled definition) ~phase:None (snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
  in
  assert changed;
  let original = original_nominal selected in
  let selected =
    edit_frontier selected ~f:(fun payload ->
      set payload "ready" (`Array [ append original "future_nominal" (`Number "2.00e1") ]))
  in
  let original = original_nominal selected in
  let intent = capture_intent definition selected original in
  let attempt = request intent "frontier-execution:0" in
  let recorded, row = E.record_execution selected ~intent ~attempt |> protocol_ok in
  [%test_eq: int] (H.pending_count (frontier selected)) 1;
  [%test_eq: int] (H.pending_count (frontier recorded)) 1;
  [%test_eq: int] (H.pending_execution_steps (frontier recorded)) 2;
  [%test_eq: int] (List.length (H.ready (frontier recorded))) 0;
  assert (D.Json.equal original (Intent.nominal_json (Execution.intent row)));
  let replay, replay_row = E.record_execution recorded ~intent ~attempt |> protocol_ok in
  assert (D.Json.equal (entry_json recorded) (entry_json replay));
  assert (Execution.equal row replay_row);
  let recorded =
    edit_frontier recorded ~f:(fun payload ->
      match field payload "executions" with
      | `Array [ wrapper ] ->
        set
          payload
          "executions"
          (`Array [ append wrapper "future_row" (`String "owned by this occurrence") ])
      | _ -> assert false)
  in
  let restored = restore recorded in
  assert (D.Json.equal (entry_json recorded) (entry_json restored));
  let paused = E.pause restored ~reason:Operator_pause |> protocol_ok in
  let retired = E.retire paused ~reason:Operator_retire |> protocol_ok in
  List.iter [ paused; retired ] ~f:(fun entry ->
    [%test_eq: int] (H.pending_count (frontier entry)) 1;
    assert (
      D.Json.equal
        (field
           (field (field (field (entry_json entry) "payload") "frontier") "payload")
           "executions")
        (field
           (field (field (field (entry_json recorded) "payload") "frontier") "payload")
           "executions")));
  let replay, _ = E.record_execution retired ~intent ~attempt |> protocol_ok in
  assert (D.Json.equal (entry_json retired) (entry_json replay));
  let no_marker =
    map_frontier (entry_json recorded) ~f:(fun document ->
      match document with
      | `Object fields ->
        `Object
          (List.filter fields ~f:(fun (name, _) ->
             not (String.equal name "required_semantics")))
      | _ -> assert false)
  in
  assert (Result.is_error (decode no_marker));
  List.iter
    [ `Null; `Array [] ]
    ~f:(fun replacement ->
      let incoming =
        map_frontier (entry_json recorded) ~f:(fun document ->
          map_payload document ~f:(fun payload -> set payload "executions" replacement))
      in
      assert (Result.is_error (decode incoming)));
  let different =
    F.create
      ~key:(F.key definition)
      ~creator:(F.creator definition)
      ~source:(source 2L)
      ~birth_lifecycle_revision:(F.birth_lifecycle_revision definition)
      ~schedule:(F.schedule definition)
      ~target:(F.target definition)
      ~policy:(F.policy definition)
      ~created_at:(F.created_at definition)
    |> protocol_ok
  in
  let different_nominal =
    N.create_interval different ~slot:0L |> protocol_ok |> N.to_json
  in
  let other_intent = capture_intent different selected different_nominal in
  let other_attempt = request other_intent "frontier-execution:other" in
  let other_row =
    Execution.create ~intent:other_intent ~attempt:other_attempt |> protocol_ok
  in
  let wrong =
    map_frontier (entry_json recorded) ~f:(fun document ->
      map_payload document ~f:(fun payload ->
        match field payload "executions" with
        | `Array [ wrapper ] ->
          set
            payload
            "executions"
            (`Array [ set wrapper "execution" (Execution.to_json other_row) ])
        | _ -> assert false))
  in
  assert (Result.is_error (decode wrong));
  let future = H.capacity_projection (frontier recorded) |> document_ok in
  let current = H.to_document (frontier recorded) |> document_ok |> D.Document.json in
  let shallow =
    List.find_map_exn
      (List.init 48 ~f:(fun index -> index + 1))
      ~f:(fun max_depth ->
        let limits =
          D.Limits.create
            ~max_bytes:(2 * 1024 * 1024)
            ~max_depth
            ~max_fields:32768
            ~max_nodes:65536
          |> document_ok
        in
        if Result.is_ok (D.Json.validate ~limits current) then Some limits else None)
  in
  assert (Result.is_ok (D.Json.validate ~limits:shallow current));
  assert (Result.is_error (D.Json.validate ~limits:shallow future));
  print_endline
    "exact Ready moves once; pending preserved; unknowns retained; old definition \
     mismatches reject; future depth charged";
  [%expect
    {| exact Ready moves once; pending preserved; unknowns retained; old definition mismatches reject; future depth charged |}]
;;

let%expect_test
    "native execution recording saturates before installation and roundtrips every \
     accepted table"
  =
  let policy =
    A.Recurrence_policy.create
      ~misfire:(Catch_up 32)
      ~grace_ms:60_000L
      ~max_pending:32
      ~max_history:1
      ~max_calendar_steps:256
    |> protocol_ok
  in
  let target =
    A.Recurrence_target.create_existing
      ~session
      ~source:(source 1L)
      ~configuration_revision:0L
      ~input:{ kind = Plain_text; text = String.make (100 * 1024) 'x'; attachments = [] }
    |> protocol_ok
  in
  let definition =
    definition ~policy "execution-bound"
    |> fun definition -> Domain_tests.replace_target definition target
  in
  let selected, _, _ =
    step
      (enabled definition)
      ~phase:None
      (snapshot ~wall:(at_ms 31_000) ~elapsed_ms:31_000)
  in
  [%test_eq: int] (H.pending_count (frontier selected)) 32;
  let rec record entry accepted =
    if accepted >= 32 then failwith "expected finite complete frontier saturation";
    let original =
      H.to_document (frontier entry)
      |> document_ok
      |> D.Document.payload
      |> fun payload ->
      match field payload "ready" with
      | `Array (first :: _) -> first
      | _ -> failwith "expected unrecorded selected work"
    in
    let intent = capture_intent definition entry original in
    let attempt = request intent ("bounded:" ^ Int.to_string accepted) in
    let before = entry_json entry in
    match E.record_execution entry ~intent ~attempt with
    | Error error ->
      assert (P.Error.equal_code error.code Invalid_request);
      [%test_eq: string]
        error.message
        (Sexp.to_string_hum
           (D.Error.sexp_of_t
              (Invalid_field
                 { path = []
                 ; reason =
                     Sexp.to_string_hum (D.Error.sexp_of_t (Limit_exceeded "bytes"))
                 })));
      assert (accepted > 1);
      assert (D.Json.equal before (entry_json entry));
      [%test_eq: int] (H.pending_count (frontier entry)) 32;
      assert (List.length (H.executions (frontier entry)) < 32)
    | Ok (next, _) ->
      let restored = restore next in
      assert (D.Json.equal (entry_json next) (entry_json restored));
      let raw_table =
        H.to_document (frontier next)
        |> document_ok
        |> D.Document.payload
        |> fun payload -> field payload "executions"
      in
      D.Json.validate ~limits:H.limits raw_table |> document_ok;
      [%test_eq: int] (H.pending_count (frontier next)) 32;
      record next (accepted + 1)
  in
  record selected 0;
  print_endline
    "native bounded prefix roundtrips; capacity rejection preserves all 32 claims and \
     original bytes";
  [%expect
    {| native bounded prefix roundtrips; capacity rejection preserves all 32 claims and original bytes |}]
;;
