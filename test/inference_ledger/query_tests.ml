open! Core
open Ledger_fixture

let json_bytes value =
  D.Json.validate_and_measure ~limits:Transcript.Admission.default value |> document_ok
;;

let%expect_test "query pages charge the actual continuation before append" =
  let t, _, _ = admit (ledger ()) in
  let t, _, _ = admit t in
  let rows =
    List.map (L.rows t) ~f:(fun row ->
      L.row_view row ~include_configuration:false ~include_diagnostics:false)
  in
  let first = List.hd_exn rows in
  let second = List.nth_exn rows 1 in
  let cursor = P.Page.Cursor.of_string "opaque-next-after-first" |> protocol_ok in
  let summary = L.summary t in
  let page =
    Q.Response.create
      ~summary
      ~attempts:{ items = [ first ]; next_cursor = Some cursor }
      ~max_bytes:(16 * 1024 * 1024)
    |> protocol_ok
  in
  let exact = json_bytes (Q.Response.to_json page) in
  let builder = Q.Response.Builder.create ~summary ~max_bytes:exact |> protocol_ok in
  let builder =
    Q.Response.Builder.add builder first ~next_cursor:(Some cursor)
    |> protocol_ok
    |> Option.value_exn
  in
  assert (
    Option.is_none (Q.Response.Builder.add builder second ~next_cursor:None |> protocol_ok));
  let result = Q.Response.Builder.finish builder |> protocol_ok in
  assert (List.length (Q.Response.attempts result).items = 1);
  assert (
    Option.equal
      P.Page.Cursor.equal
      (Q.Response.attempts result).next_cursor
      (Some cursor));
  let too_small =
    Q.Response.Builder.create ~summary ~max_bytes:(exact - 1) |> protocol_ok
  in
  assert (
    Result.is_error (Q.Response.Builder.add too_small first ~next_cursor:(Some cursor)));
  print_endline "whole row and cursor admitted atomically";
  [%expect {| whole row and cursor admitted atomically |}]
;;

let%expect_test
    "safe row projections omit diagnostics and account configuration without disclosure"
  =
  let t, _, _ = admit (ledger ()) in
  let row = List.hd_exn (L.rows t) in
  let hidden = L.row_view row ~include_configuration:false ~include_diagnostics:false in
  let raw = Jsonaf.to_string (Q.Attempt.to_json hidden) in
  assert (not (String.is_substring raw ~substring:"safe-account"));
  let detailed = L.row_view row ~include_configuration:true ~include_diagnostics:true in
  assert (
    String.is_substring
      (Jsonaf.to_string (Q.Attempt.to_json detailed))
      ~substring:"safe-account");
  assert (
    not
      (String.is_substring
         (Jsonaf.to_string (Q.Attempt.to_json detailed))
         ~substring:"private.example"));
  assert (Option.is_none (Q.Attempt.diagnostics hidden));
  assert (Option.is_some (Q.Attempt.diagnostics detailed));
  print_endline "safe detail selection retained";
  [%expect {| safe detail selection retained |}]
;;

let%expect_test
    "public reads bound original unknown JSON and enforce the new page maximum"
  =
  let t, _, _ = admit (ledger ()) in
  let summary = L.summary t in
  let raw = add (Q.Summary.to_json summary) "unknown" (`String (String.make 8192 'x')) in
  assert (Result.is_error (Q.Summary.of_json raw));
  let raw =
    Q.Request.create
      ~session_id:sid
      ~page:(P.Page.Request.create ~limit:1000 () |> protocol_ok)
      ~include_configuration:false
      ~include_diagnostics:false
    |> protocol_ok
    |> Q.Request.to_json
  in
  assert (Result.is_ok (Q.Request.of_json raw));
  assert (
    Result.is_error
      (Q.Request.create
         ~session_id:sid
         ~page:(P.Page.Request.create ~limit:1001 () |> protocol_ok)
         ~include_configuration:false
         ~include_diagnostics:false));
  print_endline "bounded original JSON and 1000-row contract";
  [%expect {| bounded original JSON and 1000-row contract |}]
;;

let%expect_test
    "retained actual sum exposes overflow and preserves per-component missing reasons"
  =
  let t, h, _ = admit (ledger ()) in
  let t, _ = L.observe t h (usage h ~revision:0L Int64.max_value) |> ledger_ok in
  let t, h, _ = admit t in
  let t, _ = L.observe t h (usage h ~revision:0L 1L) |> ledger_ok in
  let components = Q.Summary.components (L.summary t) in
  assert (Q.Metric.equal_sum components.input.actual Overflow);
  assert (Int64.equal components.cached_input.unknown.explicit_null 2L);
  assert (Int64.equal components.reported_total.unknown.not_reported 2L);
  assert (Q.Metric.equal_sum components.output.actual (Tokens 0L));
  print_endline "overflow, actual zero, null and absent remain distinct";
  [%expect {| overflow, actual zero, null and absent remain distinct |}]
;;
