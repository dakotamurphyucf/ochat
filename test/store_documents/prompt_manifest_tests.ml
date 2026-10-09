open Core
module S = Agent_store
module D = Document_schema
module P = Agent_protocol
module C = S.Prompt_manifest_document
module Pub = C.Publication

let checked result =
  Result.map_error result ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let store_ok result =
  Result.map_error result ~f:(fun e -> Sexp.to_string_hum (S.Store_error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let digest text = Digestif.SHA256.(digest_string text |> to_hex)

let revision =
  P.Id.Prompt_revision.of_string "prv_named_manifest"
  |> Result.map_error ~f:(fun e -> e.P.Error.message)
  |> Result.ok_or_failwith
;;

let fixture =
  {| {
 "format":"ochat.document", "schema_version":1, "kind":"store.prompt_manifest",
 "future_envelope":{"retained":null},
 "payload":{"version":"1","revision_id":"prv_named_manifest","root_relative_path":"root.chatmd",
 "root_sha256":"091c8f42365610e2a7e78b9b4b7ecad18030b9c051364b141b433f6f6876e743",
 "sources":[{"relative_path":"library/tools.chatmd","sha256":"147535516e5be37c00d40dd3afed5b78d612d00ab0690a465f09a0ec50aa064a","future_source":1e+00}],
 "parser_schema_version":"1","runtime_schema_version":"1","created_at":"2026-08-15T12:00:00Z","future_manifest":{"keep":null}}
 } |}
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Null -> `Null
  | Absent -> assert false
;;

let edit json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal key name then value else old))
  | _ -> assert false
;;

let%expect_test "manifest publication retains original byte digest and unknown inventory" =
  let publication =
    Pub.of_bytes fixture ~revision_id:revision ~expected_sha256:(digest fixture)
    |> store_ok
  in
  assert (String.equal fixture (Pub.bytes publication));
  let original = D.Document.decode ~limits:C.limits fixture |> checked in
  let encoded = C.to_document (Pub.document publication) |> checked in
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.json original) "future_envelope")
      (field (D.Document.json encoded) "future_envelope"));
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.payload original) "sources")
      (field (D.Document.payload encoded) "sources"));
  let authored = Pub.create (C.value (Pub.document publication)) |> checked in
  assert (not (String.equal (Pub.sha256 authored) (Pub.sha256 publication)));
  assert (String.equal (Pub.bytes publication) fixture);
  print_endline
    "original whitespace and source extensions retained; authored projection has \
     distinct digest";
  [%expect
    {|original whitespace and source extensions retained; authored projection has distinct digest|}]
;;

let%expect_test "manifest integrity and original owner precede current payload validation"
  =
  let original = D.Document.decode ~limits:C.limits fixture |> checked in
  let payload = edit (D.Document.payload original) "parser_schema_version" `Null in
  let raw = Jsonaf.to_string (edit (D.Document.json original) "payload" payload) in
  (match Pub.of_bytes raw ~revision_id:revision ~expected_sha256:(digest fixture) with
   | Error (S.Store_error.Corrupt _) -> ()
   | Ok _ | Error _ -> assert false);
  let foreign =
    P.Id.Prompt_revision.of_string "prv_foreign_manifest"
    |> Result.map_error ~f:(fun e -> e.P.Error.message)
    |> Result.ok_or_failwith
  in
  (match Pub.of_bytes raw ~revision_id:foreign ~expected_sha256:(digest raw) with
   | Error (S.Store_error.Corrupt _) -> ()
   | Ok _ | Error _ -> assert false);
  (match Pub.of_bytes raw ~revision_id:revision ~expected_sha256:(digest raw) with
   | Error (S.Store_error.Document _) -> ()
   | Ok _ | Error _ -> assert false);
  print_endline
    "original digest and directory identity fail before invalid schema counter";
  [%expect {|original digest and directory identity fail before invalid schema counter|}]
;;

let%expect_test "authored manifest byte cap and safe inventory reject before publication" =
  let known =
    Pub.of_bytes fixture ~revision_id:revision ~expected_sha256:(digest fixture)
    |> store_ok
    |> Pub.document
    |> C.value
  in
  assert (
    Result.is_error
      (Pub.create { known with canonical_source = Some (String.make C.max_bytes 'x') }));
  assert (Result.is_error (Pub.create { known with root_relative_path = "../outside" }));
  let source = List.hd_exn known.sources in
  assert (Result.is_error (Pub.create { known with sources = [ source; source ] }));
  assert (
    Result.is_error
      (Pub.create
         { known with
           sources = [ { source with relative_path = known.root_relative_path } ]
         }));
  let original = D.Document.decode ~limits:C.limits fixture |> checked in
  let json =
    match D.Document.json original with
    | `Object fields ->
      `Object
        (fields @ [ "required_semantics", `Array [ `String "unsupported-inventory" ] ])
    | _ -> assert false
  in
  let raw = Jsonaf.to_string json in
  assert (
    Result.is_error (Pub.of_bytes raw ~revision_id:revision ~expected_sha256:(digest raw)));
  print_endline
    "262144-byte inventory admission, safe paths, duplicate/root collisions and unknown \
     semantics enforced";
  [%expect
    {|262144-byte inventory admission, safe paths, duplicate/root collisions and unknown semantics enforced|}]
;;
