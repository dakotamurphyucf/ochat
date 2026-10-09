open! Core
module C = Agent_store.History_revision_conversion
module D = Document_schema

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let%expect_test "legacy attach envelopes initialize only owned history locations" =
  let old =
    Jsonaf.of_string
      {|{"replay":{"type":"snapshot","snapshot":{"canonical_history":{"entries":[{"id":"old","payload":{"content_revision":"opaque"},"evidence":17}]},"deferred_entries":[{"id":"pending"}],"effective_history":{"entries":[{"id":"effective","content_revision":"9"}]},"opaque":{"entries":[{"id":"untouched"}]}}},"receipt_evidence":"original"}|}
  in
  let converted = C.initialize_method_result old ~method_name:"session.attach" |> ok in
  print_endline (Jsonaf.to_string converted);
  printf
    "unrelated-method-unchanged=%b\n"
    (Jsonaf.exactly_equal
       old
       (C.initialize_method_result old ~method_name:"job.cancel" |> ok));
  [%expect
    {|{"replay":{"type":"snapshot","snapshot":{"canonical_history":{"entries":[{"id":"old","payload":{"content_revision":"opaque"},"evidence":17,"content_revision":"0"}]},"deferred_entries":[{"id":"pending","content_revision":"0"}],"effective_history":{"entries":[{"id":"effective","content_revision":"9"}]},"opaque":{"entries":[{"id":"untouched"}]}}},"receipt_evidence":"original"}
unrelated-method-unchanged=true|}]
;;

let%expect_test "legacy attach events preserve unknown replacement snapshot lookalikes" =
  let old =
    Jsonaf.of_string
      {|{"replay":{"type":"events","events":[{"kind":"history.appended","payload":[{"id":"a"}]},{"kind":"session.updated","payload":{"replacement_snapshot":{"canonical_history":{"entries":[{"id":"b"}]},"deferred_entries":[]}}},{"kind":"unrecognized.event","payload":{"replacement_snapshot":{"entries":[{"id":"opaque"}]}}}]}}|}
  in
  C.initialize_method_result old ~method_name:"session.attach"
  |> ok
  |> Jsonaf.to_string
  |> print_endline;
  [%expect
    {|{"replay":{"type":"events","events":[{"kind":"history.appended","payload":[{"id":"a","content_revision":"0"}]},{"kind":"session.updated","payload":{"replacement_snapshot":{"canonical_history":{"entries":[{"id":"b","content_revision":"0"}]},"deferred_entries":[]}}},{"kind":"unrecognized.event","payload":{"replacement_snapshot":{"entries":[{"id":"opaque"}]}}}]}}|}]
;;

let%expect_test "legacy optional null fields remain null" =
  let snapshot =
    Jsonaf.of_string
      {|{"canonical_history":{"entries":[]},"deferred_entries":[],"effective_history":null}|}
  in
  let event =
    Jsonaf.of_string
      {|{"kind":"session.updated","payload":{"replacement_snapshot":null}}|}
  in
  let overlay =
    Jsonaf.of_string
      {|{"kind":"moderator.overlay_changed","payload":{"effective_history":null}}|}
  in
  printf
    "snapshot=%b event=%b overlay=%b\n"
    (Jsonaf.exactly_equal snapshot (C.initialize_snapshot_history snapshot |> ok))
    (Jsonaf.exactly_equal event (C.initialize_event_history event |> ok))
    (Jsonaf.exactly_equal overlay (C.initialize_event_history overlay |> ok));
  [%expect {|snapshot=true event=true overlay=true|}]
;;
