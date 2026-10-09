open Core
module S = Agent_store
module D = Document_schema
module C = S.Delegation_document

let checked result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let fixture =
  {|{"format":"ochat.document","schema_version":6,"kind":"delegation.intent","future_envelope":{"nullable":null},"payload":{"key":{"parent_session_id":"ses_parent_named","parent_generation":"3","principal_id":"pri_named_parent","idempotency_key":"named-fixture","future_key":"kept"},"request_sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","admission":{"child_session_id":"ses_child_named","revision_id":"prv_child_named","transaction_id":"txn_named_admission","manifest_sha256":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","parent_revision_id":"prv_parent_named","authority_sha256":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc","capability_pins":[{"name":"read_file","pin":"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd","future_pin":1e+00}],"lifetime":{"kind":"owned","future_lifetime":null},"created_at":"2026-08-15T12:00:00Z","inference_target":{"adapter":"fixture.responses","profile":"selected","endpoint":"fixture://responses","model":"fixture-model","settings":[],"unknown_target":1e+00},"future_admission":{"preserve":null}},"stage":"reserved","future_disposition":"kept"}}|}
;;

let field json name =
  match D.Json.field json ~name with
  | Value json -> json
  | Null -> `Null
  | Absent -> assert false
;;

let edit json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal name key then value else old))
  | _ -> assert false
;;

let%expect_test "named delegation disposition preserves immutable admission and presence" =
  let original = D.Document.decode ~limits:C.limits fixture |> checked in
  let carrier = C.of_document original |> checked in
  let digest = C.admission_sha256 carrier in
  assert (
    String.equal
      digest
      Digestif.SHA256.(
        digest_string (Jsonaf.to_string (field (D.Document.payload original) "admission"))
        |> to_hex));
  let updated =
    C.with_disposition
      carrier
      ~stage:Artifact_installed
      ~revocation:(Some Parent_stopped)
      ~artifact_collection:(Some Prepared)
    |> checked
  in
  let document = C.to_document updated |> checked in
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.payload original) "admission")
      (field (D.Document.payload document) "admission"));
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.json original) "future_envelope")
      (field (D.Document.json document) "future_envelope"));
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.payload original) "key")
      (field (D.Document.payload document) "key"));
  assert (String.equal digest (C.admission_sha256 (C.of_document document |> checked)));
  let admission = field (D.Document.payload document) "admission" in
  assert (
    match D.Json.field admission ~name:"parent_stop_epoch" with
    | Absent -> true
    | Null | Value _ -> false);
  assert (
    match D.Json.field admission ~name:"authored_tool" with
    | Absent -> true
    | Null | Value _ -> false);
  let altered = { (C.to_record updated) with request_sha256 = String.make 64 'e' } in
  assert (Result.is_error (C.of_record altered));
  print_endline
    "original named hash, nested extensions and absent fields survive; immutable \
     substitution rejects";
  [%expect
    {|original named hash, nested extensions and absent fields survive; immutable substitution rejects|}]
;;

let%expect_test "delegation current admission requires target and supported semantics" =
  let document = D.Document.decode ~limits:C.limits fixture |> checked in
  let payload = D.Document.payload document in
  List.iter
    [ `Null; `Object [] ]
    ~f:(fun invalid ->
      let admission = edit (field payload "admission") "inference_target" invalid in
      let json =
        edit (D.Document.json document) "payload" (edit payload "admission" admission)
      in
      assert (
        Result.is_error
          (C.of_document (D.Document.inspect ~limits:C.limits json |> checked))));
  let json =
    match D.Document.json document with
    | `Object fields ->
      `Object (fields @ [ "required_semantics", `Array [ `String "unknown-authority" ] ])
    | _ -> assert false
  in
  assert (
    Result.is_error (C.of_document (D.Document.inspect ~limits:C.limits json |> checked)));
  let missing_target =
    { (C.to_record (C.of_document document |> checked)).admission with
      inference_target = None
    }
  in
  let known =
    { (C.to_record (C.of_document document |> checked)) with
      admission = missing_target
    ; preservation = None
    }
  in
  assert (Result.is_error (C.create known));
  print_endline "missing/corrupt capture and unsupported authority semantics rejected";
  [%expect {|missing/corrupt capture and unsupported authority semantics rejected|}]
;;
