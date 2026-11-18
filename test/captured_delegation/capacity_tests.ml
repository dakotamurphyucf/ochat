open! Core
open Support

let%expect_test
    "new admission charges actual framed reads, retained unknowns and all known future \
     dispositions"
  =
  with_store "ochat-delegation-capacity" (fun env _sw root store ->
    let ledger = S.delegations store in
    let candidate = admission "first" in
    let artifact = artifact candidate in
    let candidate =
      { candidate with
        manifest_sha256 = artifact.manifest_sha256
      ; captured_recurrence = Some (origin ())
      }
    in
    let initial = C.create (record "first" candidate) |> document_ok in
    let first_future = future_frame_bytes initial in
    assert (
      Result.is_error (reserve ~bytes:(first_future - 1) ledger (key "first") candidate));
    assert (not (path_exists env (Eio.Path.native_exn (file env root (key "first")))));
    let first =
      reserve ~bytes:first_future ledger (key "first") candidate |> store_ok |> row
    in
    (* Compatible unknown disposition/envelope bytes are retained. Actual frame
       whitespace is intentionally larger than its canonical JSON projection. *)
    (* Use the actual historical template, before any rewrite, so admission
       must charge absent-to-present control fields as well as string growth. *)
    let old_document =
      document first
      |> fun document ->
      change_payload document ~f:(fun payload ->
        let payload = remove payload [ "revocation"; "artifact_collection" ] in
        append
          payload
          "future_disposition"
          (`Object [ "lexeme", `Number "1e+00"; "bytes", `String (String.make 512 'u') ]))
      |> fun document ->
      append (D.Document.json document) "future_envelope" `Null |> inspect
    in
    let old = C.of_document old_document |> document_ok in
    let old_frame =
      Agent_store.Frame.encode
        ~max_payload_length:C.max_payload_length
        ~flags:0
        (" \n" ^ D.Document.to_string old_document ^ "\n")
      |> frame_ok
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) (file env root first.key) old_frame;
    let old_actual_bytes = String.length old_frame in
    let old_future = future_frame_bytes old in
    assert (old_future > old_actual_bytes);
    let second_admission = admission "second" in
    let second_carrier = C.create (record "second" second_admission) |> document_ok in
    let second_future = future_frame_bytes second_carrier in
    let missing_old_growth = old_actual_bytes + second_future in
    assert (
      Result.is_error
        (reserve ~bytes:missing_old_growth ledger (key "second") second_admission));
    assert (not (path_exists env (Eio.Path.native_exn (file env root (key "second")))));
    let budget = old_future + second_future in
    let second =
      reserve ~bytes:budget ledger (key "second") second_admission |> store_ok |> row
    in
    List.iter (future_documents old) ~f:(fun doc ->
      let carrier = C.of_document doc |> document_ok in
      let predicted = C.reserved_payload_bytes carrier |> document_ok in
      let oracle =
        future_frame_bytes carrier
        - Agent_store.Frame.header_length
        - Agent_store.Frame.checksum_length
      in
      assert (predicted = oracle));
    install env root candidate artifact;
    let first = L.advance ledger first Artifact_installed |> store_ok in
    let first = L.revoke ledger first Authority_changed |> store_ok in
    collect ledger budget;
    let first = L.find ledger first.key |> store_ok |> Option.value_exn in
    assert (Option.is_some first.artifact_collection);
    let second = L.advance ledger second Artifact_installed |> store_ok in
    let second = L.advance ledger second Child_installed |> store_ok in
    let second = L.advance ledger second Linked |> store_ok in
    let second = L.revoke ledger second Authority_changed |> store_ok in
    let actual =
      List.sum
        (module Int)
        [ first; second ]
        ~f:(fun record -> String.length (Eio.Path.load (file env root record.key)))
    in
    assert (actual <= budget);
    let restored = document first in
    assert (
      Jsonaf.exactly_equal
        (field (D.Document.payload old_document) "future_disposition")
        (field (D.Document.payload restored) "future_disposition"));
    assert (
      Jsonaf.exactly_equal (field (D.Document.json restored) "future_envelope") `Null);
    (* No candidate debit or retroactive reserve is demanded on a replay. *)
    let replay = reserve ~bytes:actual ledger first.key candidate |> store_ok |> row in
    assert (L.Reference.equal (L.reference first) (L.reference replay));
    assert (
      L.with_records ledger ~max_records ~max_bytes:actual ~f:(fun rows ->
        Ok (List.length rows))
      |> store_ok
      = 2));
  print_endline
    "candidate-only and retained-growth shortfalls reject before creation; actual \
     retained controls/collection fit the exact allowance and preserve unknowns";
  [%expect
    {|candidate-only and retained-growth shortfalls reject before creation; actual retained controls/collection fit the exact allowance and preserve unknowns|}]
;;

let%expect_test
    "per-record future headroom is admitted before storing a large captured origin"
  =
  with_store "ochat-captured-record-bound" (fun env _sw root store ->
    let ledger = S.delegations store in
    let candidate = admission "large" in
    let artifact = artifact candidate in
    let candidate padding =
      let origin =
        O.create
          ~reservation:(`Object [ "id", `String "reserved" ])
          ~definition:(`Object [ "padding", `String (String.make padding 'p') ])
          ~initial_spec_sha256:(digest "spec")
        |> document_ok
      in
      { candidate with
        manifest_sha256 = artifact.manifest_sha256
      ; captured_recurrence = Some origin
      }
    in
    let initial = C.create (record "large" (candidate 0)) |> document_ok in
    let overhead =
      future_frame_bytes initial
      - Agent_store.Frame.header_length
      - Agent_store.Frame.checksum_length
    in
    let padding = C.max_payload_length - overhead in
    let fits = candidate padding in
    let too_large = candidate (padding + 1) in
    (* Both current records fit. Only the latter lacks room for required later
       disposition; failure is before any durable candidate or artifact. *)
    ignore (document (record "large" too_large) : D.Document.t);
    assert (Result.is_error (reserve ledger (key "large") too_large));
    assert (not (path_exists env (Eio.Path.native_exn (file env root (key "large")))));
    let first = reserve ledger (key "large") fits |> store_ok |> row in
    let carrier = C.of_record first |> document_ok in
    assert (C.reserved_payload_bytes carrier |> document_ok = C.max_payload_length);
    assert (
      future_frame_bytes carrier
      = C.max_payload_length
        + Agent_store.Frame.header_length
        + Agent_store.Frame.checksum_length);
    install env root fits artifact;
    let first = L.advance ledger first Artifact_installed |> store_ok in
    let first = L.revoke ledger first Authority_changed |> store_ok in
    collect ledger max_bytes;
    let final = L.find ledger first.key |> store_ok |> Option.value_exn in
    assert (Option.is_some final.artifact_collection);
    assert (String.length (D.Document.to_string (document final)) = C.max_payload_length);
    assert (L.Reference.equal (L.reference first) (L.reference final));
    (* Simulate a valid pre-reservation-policy record. Its current encoding fits
       but the historical owner never reserved later disposition headroom.
       Reflush remains possible; a new key cannot consume nonexistent room. *)
    let historical_admission =
      { (admission "older") with captured_recurrence = too_large.captured_recurrence }
    in
    let historical = record "older" historical_admission |> document |> frame in
    Eio.Path.save ~create:(`Or_truncate 0o600) (file env root (key "older")) historical;
    let current_bytes =
      String.length (Eio.Path.load (file env root first.key)) + String.length historical
    in
    let replay =
      reserve ~bytes:current_bytes ledger (key "older") historical_admission
      |> store_ok
      |> row
    in
    assert (L.equal_stage replay.stage Reserved);
    let fresh = admission "after" in
    assert (Result.is_error (reserve ~bytes:max_bytes ledger (key "after") fresh));
    assert (not (path_exists env (Eio.Path.native_exn (file env root (key "after"))))));
  print_endline
    "one byte without future room rejects upstream; accepted custody reaches the full \
     bound; historical replay reflushes without inventing retroactive headroom";
  [%expect
    {|one byte without future room rejects upstream; accepted custody reaches the full bound; historical replay reflushes without inventing retroactive headroom|}]
;;
