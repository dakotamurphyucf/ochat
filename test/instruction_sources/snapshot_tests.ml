open! Core
open Support

let snapshot values = C.Snapshot.capture values ~limits ~checkpoint:ignore |> content_ok
let restore stored = C.Snapshot.of_storage stored ~limits ~checkpoint:ignore

let with_metadata stored f =
  let json =
    Storage.Manifest.metadata (Storage.manifest stored) |> D.Document.json |> f
  in
  let metadata = D.Document.inspect ~limits:D.Limits.default json |> doc_ok in
  Storage.create ~metadata ~payloads:(Storage.payloads stored) |> storage_ok
;;

let replace object_ name value =
  match object_ with
  | `Object fields ->
    `Object
      ((name, value)
       :: List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
  | _ -> failwith "expected object"
;;

let map_rows json f =
  match D.Json.field json ~name:"payload" with
  | Value payload ->
    (match D.Json.field payload ~name:"instructions" with
     | Value (`Array rows) ->
       replace json "payload" (replace payload "instructions" (`Array (f rows)))
     | _ -> failwith "expected instruction array")
  | _ -> failwith "expected document payload"
;;

let%expect_test
    "ordered qualified revisions use existing payload custody and preserve unknown fields"
  =
  let text = skill "demo" in
  let values =
    [ loaded ~root:"left" "demo/SKILL.md" text
    ; loaded ~root:"right" "demo/SKILL.md" text
    ]
  in
  let stored = snapshot values |> C.Snapshot.to_storage in
  let stored =
    with_metadata stored (fun json ->
      replace
        (map_rows
           json
           (List.map ~f:(fun row -> replace row "future_note" (`String "retain"))))
        "future_envelope"
        (`String "retain"))
  in
  let replayed = restore stored |> content_ok in
  let labels =
    C.Snapshot.loaded replayed
    |> List.map ~f:(fun value ->
      C.Root_id.to_string (C.Id.root (C.Observed.id (C.Loaded.observed value))))
  in
  print_s [%sexp (labels : string list), (List.length (Storage.payloads stored) : int)];
  assert (
    D.Json.equal
      (D.Document.json (Storage.Manifest.metadata (Storage.manifest stored)))
      (D.Document.json
         (Storage.Manifest.metadata (Storage.manifest (C.Snapshot.to_storage replayed)))));
  List.iter (C.Snapshot.loaded replayed) ~f:(fun value ->
    [%test_eq: string] (C.Loaded.contents value) text);
  [%expect {| ((left right) 1) |}]
;;

let%test_unit
    "capture and restore reject conflicting identities and payload/assertion corruption"
  =
  let value = loaded "demo/SKILL.md" (skill "demo") in
  assert (Result.is_error (C.Snapshot.capture [ value; value ] ~limits ~checkpoint:ignore));
  let stored = snapshot [ value ] |> C.Snapshot.to_storage in
  let metadata = Storage.Manifest.metadata (Storage.manifest stored) in
  let changed = Storage.Payload.create "different bytes" |> storage_ok in
  List.iter
    [ []; [ changed ]; changed :: Storage.payloads stored ]
    ~f:(fun payloads ->
      let corrupted = Storage.create ~metadata ~payloads |> storage_ok in
      assert (Result.is_error (restore corrupted)));
  List.iter
    [ "key", `String "wrong"
    ; "digest", `String (String.make 64 '0')
    ; "byte_length", `Number "0"
    ; "body_start_byte", `Number "0"
    ; "root", `String "../escape"
    ]
    ~f:(fun (field, value) ->
      let corrupted =
        with_metadata stored (fun json ->
          map_rows json (List.map ~f:(fun row -> replace row field value)))
      in
      assert (Result.is_error (restore corrupted)));
  let duplicate =
    with_metadata stored (fun json -> map_rows json (fun rows -> rows @ rows))
  in
  assert (Result.is_error (restore duplicate));
  let too_new =
    with_metadata stored (fun json -> replace json "schema_version" (`Number "2"))
  in
  assert (Result.is_error (restore too_new));
  let semantics =
    with_metadata stored (fun json ->
      replace json "required_semantics" (`Array [ `String "future-required" ]))
  in
  assert (Result.is_error (restore semantics))
;;

let%test_unit "exact aggregate limit including empty trailing files roundtrips" =
  let text = "root" in
  let values =
    [ loaded ~kind:Project_guidance "AGENTS.md" text
    ; loaded ~kind:Project_guidance "child/AGENTS.md" ""
    ]
  in
  let exact =
    C.Limits.create
      ~max_files:2
      ~max_file_bytes:4
      ~max_total_bytes:4
      ~max_work_steps:100_000
    |> Or_error.ok_exn
  in
  let stored =
    C.Snapshot.capture values ~limits:exact ~checkpoint:ignore
    |> content_ok
    |> C.Snapshot.to_storage
  in
  let restored =
    C.Snapshot.of_storage stored ~limits:exact ~checkpoint:ignore |> content_ok
  in
  [%test_eq: string list]
    (List.map (C.Snapshot.loaded restored) ~f:C.Loaded.contents)
    [ "root"; "" ];
  let too_small =
    C.Limits.create
      ~max_files:2
      ~max_file_bytes:4
      ~max_total_bytes:3
      ~max_work_steps:100_000
    |> Or_error.ok_exn
  in
  assert (
    Result.is_error (C.Snapshot.of_storage stored ~limits:too_small ~checkpoint:ignore))
;;
