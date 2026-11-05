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

let%expect_test "pure rotation preserves complete custody and advances each carrier once" =
  let module Catalog = A.Recurrence_catalog in
  let module Resolution = A.Recurrence_retry_resolution in
  let module Growth = A.Capacity_growth in
  let definition = definition "rotation" in
  let selected, _, _ =
    step (enabled definition) ~phase:None (snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
  in
  let intent = capture_intent definition selected (original_nominal selected) in
  let attempt number key =
    Attempt.create
      ~intent
      ~number
      ~principal_id
      ~expected_revision:(Int64.of_int (number + 7))
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
  in
  let resolution previous revision =
    Resolution.create
      ~intent
      ~attempt:previous
      ~target_revision:revision
      ~resolved_at:timestamp
    |> protocol_ok
  in
  let previous = attempt 0 "rotation:0" in
  let recorded, initial =
    E.record_execution selected ~intent ~attempt:previous |> protocol_ok
  in
  let audited =
    Execution.record_rejection initial ~code:Conflict
    |> protocol_ok
    |> fun row -> Execution.mark_uncertain row ~code:Persistence_error |> protocol_ok
  in
  let raw =
    Execution.to_json audited
    |> fun json ->
    append json "future_execution" (`Number "1.00e2")
    |> map_payload ~f:(fun payload ->
      match field payload "attempts" with
      | `Array [ first ] ->
        set
          payload
          "attempts"
          (`Array [ append first "future_attempt" (`Number "2.00e1") ])
      | _ -> assert false)
  in
  let replace_execution entry json =
    edit_frontier entry ~f:(fun payload ->
      match field payload "executions" with
      | `Array [ wrapper ] ->
        set
          payload
          "executions"
          (`Array
              [ (set wrapper "execution" json
                 |> fun wrapper -> set wrapper "future_wrapper" (`String "retained"))
              ])
      | _ -> assert false)
  in
  let recorded = replace_execution recorded raw in
  let catalog =
    Catalog.of_jsonaf
      (`Object
          [ "revision", `String "3"
          ; ( "entries"
            , `Array
                [ `Object
                    [ "id", P.Id.Recurrence.to_json (E.id recorded)
                    ; "document", entry_json recorded
                    ]
                ] )
          ; "receipts", `Array []
          ])
    |> protocol_ok
  in
  let find catalog = Catalog.find catalog (E.id recorded) |> Option.value_exn in
  let catalog_json catalog = Catalog.to_jsonaf catalog |> protocol_ok in
  let settled = resolution previous 7L in
  let next = attempt 1 "rotation:1" in
  let rotated, row =
    Catalog.rotate_execution catalog ~intent ~previous ~resolution:settled ~next
    |> protocol_ok
  in
  let updated = find rotated in
  [%test_eq: int64] (Int64.succ (Catalog.revision catalog)) (Catalog.revision rotated);
  [%test_eq: int64]
    (Int64.succ (H.revision (frontier recorded)))
    (H.revision (frontier updated));
  assert (A.Recurrence_cursor.equal (E.cursor recorded) (E.cursor updated));
  [%test_eq: int64] (E.control_revision recorded) (E.control_revision updated);
  [%test_eq: int] 1 (H.pending_count (frontier updated));
  assert (H.Summary.equal (H.summary (frontier recorded)) (H.summary (frontier updated)));
  assert (List.is_empty (H.ready (frontier updated)));
  let wrapper =
    match
      field
        (D.Document.payload (H.to_document (frontier updated) |> document_ok))
        "executions"
    with
    | `Array [ wrapper ] -> wrapper
    | _ -> assert false
  in
  assert (D.Json.equal (field wrapper "future_wrapper") (`String "retained"));
  assert (
    D.Json.equal (field (Execution.to_json row) "future_execution") (`Number "1.00e2"));
  let first = List.hd_exn (Execution.attempts row) in
  assert (
    Option.equal P.Error.equal_code (Some Conflict) (Execution.Attempt.rejection first));
  assert (
    Option.equal
      P.Error.equal_code
      (Some Persistence_error)
      (Execution.Attempt.uncertainty first));
  assert (
    Option.equal
      Resolution.equal
      (Some settled)
      (Execution.Attempt.retry_resolution first));
  let raw_first =
    match field (field (Execution.to_json row) "payload") "attempts" with
    | `Array (first :: _) -> first
    | _ -> assert false
  in
  assert (D.Json.equal (field raw_first "future_attempt") (`Number "2.00e1"));
  let replay, replay_row =
    Catalog.rotate_execution rotated ~intent ~previous ~resolution:settled ~next
    |> protocol_ok
  in
  assert (phys_equal replay rotated);
  assert (Execution.equal replay_row row);
  assert (D.Json.equal (catalog_json rotated) (catalog_json replay));
  let paused = E.pause updated ~reason:Operator_pause |> protocol_ok in
  let paused_replay, _ =
    E.rotate_execution paused ~intent ~previous ~resolution:settled ~next |> protocol_ok
  in
  assert (phys_equal paused paused_replay);
  let invalid (result : (_, P.Error.t) result) =
    match result with
    | Error error -> [%test_eq: P.Error.code] Invalid_request error.code
    | Ok _ -> failwith "invalid pure rotation was admitted"
  in
  invalid
    (Catalog.rotate_execution
       rotated
       ~intent
       ~previous
       ~resolution:(resolution previous 8L)
       ~next);
  invalid
    (Catalog.rotate_execution
       rotated
       ~intent
       ~previous:(attempt 0 "rotation:other")
       ~resolution:settled
       ~next);
  invalid
    (E.rotate_execution
       paused
       ~intent
       ~previous:next
       ~resolution:(resolution next 8L)
       ~next:(attempt 2 "rotation:2"));
  let saturated, row =
    List.fold
      (List.init 6 ~f:(fun i -> i + 2))
      ~init:(rotated, row)
      ~f:(fun (catalog, row) number ->
        let previous = Execution.Attempt.request (Execution.current_attempt row) in
        let next = attempt number ("rotation:" ^ Int.to_string number) in
        let incoming, row =
          Catalog.rotate_execution
            catalog
            ~intent
            ~previous
            ~resolution:(resolution previous (Int64.of_int (number + 6)))
            ~next
          |> protocol_ok
        in
        [%test_eq: int64]
          (Int64.succ (Catalog.revision catalog))
          (Catalog.revision incoming);
        [%test_eq: int] 1 (H.pending_count (frontier (find incoming)));
        incoming, row)
  in
  [%test_eq: int] 8 (List.length (Execution.attempts row));
  let unchanged = catalog_json saturated in
  let last = Execution.Attempt.request (Execution.current_attempt row) in
  invalid
    (Attempt.create
       ~intent
       ~number:8
       ~principal_id
       ~expected_revision:15L
       ~key:(P.Idempotency_key.of_string "rotation:8" |> protocol_ok));
  (* A different request at ordinal seven is a duplicate-ordinal replacement,
     not an exercise of an independently reachable ninth append. *)
  invalid
    (Catalog.rotate_execution
       saturated
       ~intent
       ~previous:last
       ~resolution:(resolution last 20L)
       ~next:(attempt 7 "rotation:duplicate-ordinal"));
  invalid (Catalog.rotate_execution saturated ~intent ~previous ~resolution:settled ~next);
  assert (D.Json.equal unchanged (catalog_json saturated));
  let padded = append raw "padding" (`String "") in
  let bounded = Execution.of_json padded |> protocol_ok in
  let measured = Growth.measure padded ~limits:Execution.limits |> protocol_ok in
  let reserve = Execution.reserved_growth bounded |> protocol_ok in
  let padding =
    D.Limits.max_bytes Execution.limits
    - Growth.encoded_bytes measured
    - Growth.encoded_bytes reserve
    - 1
  in
  let near_full =
    replace_execution recorded (set padded "padding" (`String (String.make padding 'x')))
  in
  let unchanged = entry_json near_full in
  invalid (E.rotate_execution near_full ~intent ~previous ~resolution:settled ~next);
  assert (D.Json.equal unchanged (entry_json near_full));
  [%test_eq: int] 1 (H.pending_count (frontier near_full));
  print_endline
    "one rotation advances catalog and frontier once; exact duplicate retains both";
  print_endline
    "eight attempts retained; ordinal eight, duplicate ordinal, stale and capacity reject";
  [%expect
    {|
    one rotation advances catalog and frontier once; exact duplicate retains both
    eight attempts retained; ordinal eight, duplicate ordinal, stale and capacity reject
    |}]
;;
