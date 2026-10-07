open! Core
open Ledger_fixture

let%expect_test "actual admission and authoritative replacement are not additive" =
  let limits = limits () in
  let t, h, _ = admit (ledger ~limits ()) in
  let t, _ = L.observe t h (usage h ~revision:0L 7L) |> ledger_ok in
  let t, d = L.observe t h (usage h ~revision:1L 11L) |> ledger_ok in
  assert (L.equal_observation_disposition d Replaced);
  let revision = L.revision t in
  let duplicate, d = L.observe t h (usage h ~revision:1L 11L) |> ledger_ok in
  assert (
    L.equal_observation_disposition d Duplicate
    && Int64.equal (L.revision duplicate) revision);
  assert (Result.is_error (L.observe t h (usage h ~revision:1L 12L)));
  let stale, d = L.observe t h (usage h ~revision:0L 99L) |> ledger_ok in
  assert (
    L.equal_observation_disposition d Stale && Int64.equal (L.revision stale) revision);
  let t = roundtrip t ~limits in
  let metric = (Q.Summary.components (L.summary t)).input in
  print_s [%sexp (metric.actual : Q.Metric.sum), (metric.actual_attempts : int64)];
  [%expect {| ((Tokens 11) 1) |}]
;;

let%expect_test
    "active retention limit is explicitly untracked and late observations never add"
  =
  let limits = limits ~attempts:1 () in
  let t, h, _ = admit (ledger ~limits ()) in
  let t, untracked, status = admit t in
  assert (L.equal_tracking status (Untracked Attempt_count));
  let t, d = L.observe t untracked (usage untracked ~revision:0L 100L) |> ledger_ok in
  assert (L.equal_observation_disposition d Ignored_untracked);
  let t = L.set_state t h interrupted |> ledger_ok in
  let t, new_handle, status = admit t in
  assert (L.equal_tracking status Tracked);
  assert (Int64.equal (L.Handle.ordinal new_handle) 3L);
  let t, d = L.observe t h (usage h ~revision:1L 100L) |> ledger_ok in
  assert (L.equal_observation_disposition d Ignored_retired);
  let coverage = Q.Summary.coverage (L.summary (roundtrip t ~limits)) in
  print_s
    [%sexp (coverage.retired_attempts : int64), (coverage.untracked_attempts : int64)];
  [%expect {| (1 1) |}]
;;

let%expect_test "prepared may end directly, exact ended repeats and reopening conflicts" =
  let t, h, _ = admit (ledger ()) in
  let terminal =
    Inference.Event.Terminal.create
      ~scope:(L.Handle.scope h)
      ~delivery:Definitely_not_submitted
      ~outcome:(Failed (Authentication Missing))
    |> Result.ok_or_failwith
  in
  let t = L.set_state t h (Terminal terminal) |> ledger_ok in
  assert (
    Int64.equal
      (L.revision t)
      (L.revision (L.set_state t h (Terminal terminal) |> ledger_ok)));
  assert (Result.is_error (L.set_state t h Running));
  let t = L.observe t h (usage h ~revision:3L 0L) |> ledger_ok |> fst in
  assert (Q.Metric.equal_sum (Q.Summary.components (L.summary t)).input.actual (Tokens 0L));
  print_endline "actual zero and pre-run failure retained";
  [%expect {| actual zero and pre-run failure retained |}]
;;

let%expect_test "future row fields survive updates and prevent unsafe retirement" =
  let limits = limits ~attempts:1 () in
  let t, h, _ = admit (ledger ~limits ()) in
  let raw =
    patch_rows (document_json t) ~f:(fun row -> add row "future_row" (`Number "1e+00"))
  in
  let t = captured raw ~limits in
  let t = L.set_state t h interrupted |> ledger_ok in
  let t, _, status = admit t in
  assert (L.equal_tracking status (Untracked Protected_future_data));
  let raw = D.Document.to_string (L.to_document t |> ledger_ok) in
  assert (String.is_substring raw ~substring:"\"future_row\":1e+00");
  assert (List.length (L.rows t) = 1);
  print_endline "protected future data remains captured";
  [%expect {| protected future data remains captured |}]
;;

let%expect_test
    "nested observation extensions are projected safely and retained through replacement"
  =
  let limits = limits () in
  let t, h, _ = admit (ledger ~limits ()) in
  let t, _ = L.observe t h (usage h ~revision:0L 1L) |> ledger_ok in
  let raw =
    patch_rows (document_json t) ~f:(fun row ->
      match D.Json.field row ~name:"record" with
      | Value record ->
        (match D.Json.field record ~name:"observations" with
         | Value (`Array observations) ->
           replace
             row
             "record"
             (replace
                record
                "observations"
                (`Array
                    (List.map observations ~f:(fun observation ->
                       add observation "future_observation" (`String "retained")))))
         | _ -> failwith "observations")
      | _ -> failwith "record")
  in
  let t = captured raw ~limits in
  let t, _ = L.observe t h (usage h ~revision:1L 2L) |> ledger_ok in
  assert (
    String.is_substring
      (D.Document.to_string (L.to_document t |> ledger_ok))
      ~substring:"future_observation");
  print_endline "future observation metadata survives";
  [%expect {| future observation metadata survives |}]
;;

let%expect_test "diagnostic ring overflow omits evidence with bounded counter headroom" =
  let t, h, _ = admit (ledger ()) in
  let diagnostic =
    O.Diagnostic.create
      ~phase:Stream
      ~reason:Timeout
      ~delivery:(Some Possibly_submitted)
      ~elapsed_ms:None
    |> observation_ok
  in
  let t =
    List.fold (List.init 17 ~f:Fn.id) ~init:t ~f:(fun t n ->
      let incoming =
        O.create
          ~scope:(L.Handle.scope h)
          ~id:
            (O.Observation_id.of_string ("diagnostic:" ^ Int.to_string n)
             |> observation_ok)
          ~revision:0L
          ~payload:(Diagnostic diagnostic)
          ~limits:O.Admission.diagnostic
        |> observation_ok
      in
      fst (L.observe t h incoming |> ledger_ok))
  in
  let record = L.Row.record (L.find t ~ordinal:1L |> Option.value_exn) in
  assert (List.length (O.Attempt_record.observations record) = 16);
  print_s [%sexp (O.Attempt_record.omitted_diagnostics record : int64)];
  [%expect {| 1 |}]
;;

let%expect_test "untracked metadata admissions reserve counter growth without a row" =
  let limits = limits ~bytes:1000 () in
  let t =
    List.fold (List.init 110 ~f:Fn.id) ~init:(ledger ~limits ()) ~f:(fun t _ ->
      let t, _, tracking = admit t in
      assert (L.equal_tracking tracking (Untracked Retained_bytes));
      assert (List.is_empty (L.rows t));
      let size =
        D.Json.validate_and_measure ~limits:(bounds (1024 * 1024)) (document_json t)
        |> document_ok
      in
      assert (size <= 1000);
      t)
  in
  let restored = roundtrip t ~limits in
  let coverage = Q.Summary.coverage (L.summary restored) in
  print_s [%sexp (coverage.untracked_attempts : int64)];
  [%expect {| 110 |}]
;;

let%expect_test
    "actual host turn commits are separate from inference terminal observations"
  =
  let timestamp = P.Timestamp.of_string "2026-10-07T00:00:00Z" |> protocol_ok in
  let operation : P.Operation.t =
    { id = P.Id.Operation.of_string "op_actual" |> protocol_ok
    ; generation = 0
    ; kind = Turn User_submit
    ; state = Starting
    ; started_at = timestamp
    ; updated_at = timestamp
    }
  in
  let t, turn, _ = L.admit_turn (ledger ()) operation |> ledger_ok in
  let repeated, _, _ = L.admit_turn t operation |> ledger_ok in
  assert (Int64.equal (L.revision t) (L.revision repeated));
  let t, h, _ = admit t in
  let terminal =
    Inference.Event.Terminal.create
      ~scope:(L.Handle.scope h)
      ~delivery:Response_started
      ~outcome:Completed
    |> Result.ok_or_failwith
  in
  let t = L.set_state t h (Terminal terminal) |> ledger_ok in
  assert (Int64.equal (Q.Summary.turns (L.summary t)).pending 1L);
  assert (Int64.equal (Q.Summary.turns (L.summary t)).completed 0L);
  let completed = { operation with state = Completed } in
  let t = L.finish_turn t turn completed |> ledger_ok in
  assert (
    Int64.equal (L.revision t) (L.revision (L.finish_turn t turn completed |> ledger_ok)));
  assert (Result.is_error (L.admit_turn t operation));
  assert (
    Result.is_error
      (L.admit_turn
         t
         { operation with
           id = P.Id.Operation.of_string "op_compaction" |> protocol_ok
         ; kind = Compaction
         }));
  print_s [%sexp ((Q.Summary.turns (L.summary t)).completed : int64)];
  [%expect {| 1 |}]
;;

let%expect_test "unknown HTTP status on a non-HTTP failure stays unowned" =
  let limits = limits () in
  let t, h, _ = admit (ledger ~limits ()) in
  let terminal =
    Inference.Event.Terminal.create
      ~scope:(L.Handle.scope h)
      ~delivery:Definitely_not_submitted
      ~outcome:(Failed (Authentication Missing))
    |> Result.ok_or_failwith
  in
  let t = L.set_state t h (Terminal terminal) |> ledger_ok in
  let raw =
    patch_rows (document_json t) ~f:(fun row ->
      let member json name =
        match D.Json.field json ~name with
        | Value value -> value
        | _ -> failwith name
      in
      let record = member row "record" in
      let state = member record "state" in
      let terminal = member state "terminal" in
      let outcome = member terminal "outcome" in
      let failure = member outcome "failure" in
      replace
        row
        "record"
        (replace
           record
           "state"
           (replace
              state
              "terminal"
              (replace
                 terminal
                 "outcome"
                 (replace outcome "failure" (add failure "status" (`Number "123")))))))
  in
  let t = captured raw ~limits in
  let t, _ = L.observe t h (usage h ~revision:1L 0L) |> ledger_ok in
  assert (
    String.is_substring
      (D.Document.to_string (L.to_document t |> ledger_ok))
      ~substring:"\"status\":123");
  print_endline "unrecognized status survives terminal metadata edits";
  [%expect {| unrecognized status survives terminal metadata edits |}]
;;

let%expect_test
    "captured ledger cannot bypass generation and retained turn identity fences"
  =
  let limits = limits () in
  let t, h, _ = admit (ledger ~limits ()) in
  let raw = document_json t in
  let payload =
    match D.Json.field raw ~name:"payload" with
    | Value value -> value
    | _ -> failwith "payload"
  in
  let raw = replace raw "payload" (replace payload "generation" (`Number "1")) in
  let document = D.Document.inspect ~limits:(bounds (1024 * 1024)) raw |> document_ok in
  assert (Result.is_error (L.of_document document ~limits));
  let t = L.set_state t h interrupted |> ledger_ok in
  ignore (L.with_generation t ~generation:1 |> ledger_ok : L.t);
  let make_operation id =
    P.Operation.
      { id = P.Id.Operation.of_string id |> protocol_ok
      ; generation = 0
      ; kind = Turn User_submit
      ; state = Running
      ; started_at = P.Timestamp.of_string "2026-10-07T00:00:00Z" |> protocol_ok
      ; updated_at = P.Timestamp.of_string "2026-10-07T00:00:00Z" |> protocol_ok
      }
  in
  let first = make_operation "op_first" in
  let second = make_operation "op_second" in
  let t, _, _ = L.admit_turn t first |> ledger_ok in
  let t, _, _ = L.admit_turn t second |> ledger_ok in
  let raw = document_json t in
  let payload =
    match D.Json.field raw ~name:"payload" with
    | Value value -> value
    | _ -> failwith "payload"
  in
  let turns =
    match D.Json.field payload ~name:"turns" with
    | Value (`Array turns) -> turns
    | _ -> failwith "turns"
  in
  let duplicate =
    List.map turns ~f:(fun turn ->
      let operation =
        match D.Json.field turn ~name:"operation" with
        | Value value -> value
        | _ -> failwith "operation"
      in
      replace turn "operation" (replace operation "id" (P.Id.Operation.to_json first.id)))
  in
  let raw = replace raw "payload" (replace payload "turns" (`Array duplicate)) in
  let document = D.Document.inspect ~limits:(bounds (1024 * 1024)) raw |> document_ok in
  assert (Result.is_error (L.of_document document ~limits));
  print_endline "captured domain uses the same active-generation and turn-identity fences";
  [%expect {| captured domain uses the same active-generation and turn-identity fences |}]
;;
