open! Core
open Support

(* An existing native generated admission, before the captured source kind was
   introduced. The literal is deliberately independent of the current encoder. *)
let old_admission =
  Jsonaf.of_string
    {|{"child_session_id":"ses_child_old","revision_id":"prv_child_old","transaction_id":"txn_child_old","manifest_sha256":"8019929504a30e49bc2405544305eaeb95236dd7d26fd21a25deb04ac670cd67","parent_revision_id":"prv_parent","parent_stop_epoch":null,"authority_sha256":"3d589133233271a0e2b6d2b756568af1f575ac41fbae56abf6b2d5c2290a224a","authored_tool":null,"capability_pins":[],"lifetime":{"kind":"owned"},"created_at":"2026-08-15T12:00:00.000000000Z","inference_target":{"adapter":"fixture.responses","profile":"selected","endpoint":"fixture://responses","model":"fixture-model","settings":[]}}|}
;;

let%expect_test "captured source custody is exact, bounded and fenced from older readers" =
  let captured = origin () in
  let captured_again =
    O.to_document captured |> document_ok |> O.of_document |> document_ok
  in
  assert (O.equal captured captured_again);
  assert (O.equal captured (O.t_of_sexp (O.sexp_of_t captured)));
  assert (
    Jsonaf.exactly_equal (field (O.reservation captured) "future_row") (`Number "1e+00"));
  let invalid_reservation = `Object [ "large", `String (String.make 65536 'x') ] in
  let deep_definition =
    List.init 80 ~f:Fn.id |> List.fold ~init:`Null ~f:(fun json _ -> `Array [ json ])
  in
  List.iter
    [ invalid_reservation, `Null; `Null, deep_definition ]
    ~f:(fun (reservation, definition) ->
      assert (
        Result.is_error
          (O.create ~reservation ~definition ~initial_spec_sha256:(digest "spec"))));
  assert (
    Result.is_error
      (O.create ~reservation:`Null ~definition:`Null ~initial_spec_sha256:"wrong"));
  assert (Or_error.is_error (Or_error.try_with (fun () -> O.t_of_sexp (Sexp.Atom "{}"))));
  let candidate = { (admission "captured") with captured_recurrence = Some captured } in
  let carrier = C.create (record "captured" candidate) |> document_ok in
  let original = C.to_document carrier |> document_ok in
  let raw = D.Document.json original in
  let marker_first =
    match raw with
    | `Object fields ->
      let marker, rest =
        List.partition_tf fields ~f:(fun (name, _) ->
          String.equal name "required_semantics")
      in
      `Object (marker @ rest)
    | _ -> assert false
  in
  let unchanged =
    decode marker_first |> C.to_document |> document_ok |> D.Document.json
  in
  assert (Jsonaf.exactly_equal marker_first unchanged);
  assert (
    List.mem
      (D.Document.required_semantics original)
      "delegation.captured-recurrence.v1"
      ~equal:String.equal);
  let rejects json = assert (Result.is_error (C.of_document (inspect json))) in
  rejects (remove raw [ "required_semantics" ]);
  rejects (set raw "required_semantics" (`Array [ `String "future.source-kind" ]));
  (* A new child session can use the controller's already captured source.
     The separate upper factory still verifies actual target/manifest custody. *)
  let same_source = { candidate with parent_revision_id = candidate.revision_id } in
  ignore (C.create (record "same-source" same_source) |> document_ok : C.t);
  List.iter [ None; Some authored ] ~f:(fun authored_tool ->
    assert (
      Result.is_error
        (C.create
           (record
              "same-source-legacy"
              { same_source with captured_recurrence = None; authored_tool }))));
  assert (
    Result.is_error
      (C.create
         (record
            "same-session"
            { same_source with child_session_id = (key "same-session").parent_session_id })));
  let contradiction = { candidate with authored_tool = Some authored } in
  assert (Result.is_error (C.create (record "contradiction" contradiction)));
  rejects
    (set
       raw
       "payload"
       (set
          (D.Document.payload original)
          "admission"
          (set
             (field (D.Document.payload original) "admission")
             "authored_tool"
             (`Object
                 [ "name", `String authored.name
                 ; "source_sha256", `String authored.source_sha256
                 ]))));
  let updated =
    C.of_document original
    |> document_ok
    |> fun t ->
    C.with_disposition
      t
      ~stage:Artifact_installed
      ~revocation:(Some Parent_stopped)
      ~artifact_collection:(Some Prepared)
    |> document_ok
    |> C.to_document
    |> document_ok
  in
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.payload original) "admission")
      (field (D.Document.payload updated) "admission"));
  let restored = C.of_document updated |> document_ok |> C.to_record in
  assert (Option.equal O.equal restored.admission.captured_recurrence (Some captured));
  print_endline
    "exact nested/envelope custody and validated sexp survive disposition; bounds, \
     source marker and contradictory kind reject";
  [%expect
    {|exact nested/envelope custody and validated sexp survive disposition; bounds, source marker and contradictory kind reject|}]
;;

let%expect_test
    "absent native and historical source fields retain their original admission hashes"
  =
  let generated = C.create (record "old" (admission "old")) |> document_ok in
  let native = C.to_document generated |> document_ok in
  [%test_result: string]
    (Jsonaf.to_string (field (D.Document.payload native) "admission"))
    ~expect:(Jsonaf.to_string old_admission);
  assert (
    String.equal (C.admission_sha256 generated) (digest (Jsonaf.to_string old_admission)));
  let authored_admission =
    set
      old_admission
      "authored_tool"
      (`Object
          [ "name", `String authored.name
          ; "source_sha256", `String authored.source_sha256
          ])
  in
  let authored_carrier =
    C.create (record "old" { (admission "old") with authored_tool = Some authored })
    |> document_ok
  in
  assert (
    String.equal
      (C.admission_sha256 authored_carrier)
      (digest (Jsonaf.to_string authored_admission)));
  let historical_admission =
    remove old_admission [ "authored_tool"; "parent_stop_epoch" ]
    |> fun json ->
    append
      json
      "future_admission"
      (`Object [ "numeric", `Number "1e+00"; "absent_is_not_null", `Null ])
  in
  let historical =
    change_payload native ~f:(fun payload -> set payload "admission" historical_admission)
    |> fun d -> append (D.Document.json d) "future_envelope" (`Number "2e+00") |> decode
  in
  let hash = digest (Jsonaf.to_string historical_admission) in
  assert (String.equal hash (C.admission_sha256 historical));
  let next =
    C.with_disposition
      historical
      ~stage:Linked
      ~revocation:(Some Authority_changed)
      ~artifact_collection:None
    |> document_ok
    |> C.to_document
    |> document_ok
  in
  assert (
    Jsonaf.exactly_equal
      historical_admission
      (field (D.Document.payload next) "admission"));
  assert (String.equal hash (C.admission_sha256 (C.of_document next |> document_ok)));
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.json next) "future_envelope")
      (`Number "2e+00"));
  print_endline
    "new generated/authored encodings keep old hashes; historical absence, unknowns and \
     numeric lexemes remain immutable";
  [%expect
    {|new generated/authored encodings keep old hashes; historical absence, unknowns and numeric lexemes remain immutable|}]
;;
