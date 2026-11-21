open! Core
module C = Recovery_notes.Catalog
module D = Document_schema
module P = Agent_protocol

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected error"
;;

let owner session =
  P.Session_ref.create
    ~server_id:(ok (P.Id.Server.of_string "srv_notes"))
    ~session_id:(ok (P.Id.Session.of_string session))
;;

let session = owner "ses_notes"
let empty () = ok (C.empty ~owner:session)
let body text = ok (C.Content.of_string text)
let path name = ok (C.Note_path.of_string name)

let client ?(principal = "pri_alice") key =
  C.Mutation_identity.Client
    { principal = ok (P.Id.Principal.of_string principal)
    ; key = ok (P.Idempotency_key.of_string key)
    }
;;

let native invocation =
  C.Mutation_identity.Native (ok (P.Id.Invocation.of_string invocation))
;;

let create catalog name content =
  C.Create
    { expected_catalog_revision = C.revision catalog
    ; path = path name
    ; content = body content
    }
;;

let applied catalog identity operation =
  match ok (C.apply catalog ~identity operation) with
  | Applied (catalog, receipt) -> catalog, receipt
  | Replayed _ -> failwith "unexpected replay"
;;

let expect_replay catalog identity operation receipt =
  match ok (C.apply catalog ~identity operation) with
  | Replayed actual -> assert (C.Receipt.equal actual receipt)
  | Applied _ -> failwith "retry committed twice"
;;

let expect_mutation_error catalog identity operation expected =
  match C.apply catalog ~identity operation with
  | Error actual -> assert (C.equal_mutation_error expected actual)
  | Ok _ -> failwith "unexpected mutation"
;;

let expect_read_error catalog selection expected =
  match C.read catalog selection with
  | Error actual -> assert (C.equal_read_error expected actual)
  | Ok _ -> failwith "unexpected read"
;;

let read catalog selection = C.Content.to_string (ok (C.read catalog selection))

let round_trip catalog =
  C.to_document catalog
  |> ok
  |> D.Document.to_string
  |> D.Document.decode ~limits:D.Limits.default
  |> ok
  |> C.of_document ~owner:session
  |> ok
;;

let%expect_test
    "an exact revision can be read immediately and a retry returns its receipt"
  =
  let start = empty () in
  let create_identity = client "create" in
  let create_request = create start "review/work.md" "first line" in
  let initial, created = applied start create_identity create_request in
  let append_identity = native "inv_append" in
  let append = C.Append { expected = created.selection; content = body "\r\nβé" } in
  let appended, receipt = applied initial append_identity append in
  printf
    "catalog revision %Ld, note revision %Ld, exact text %b\n"
    (C.Catalog_revision.to_int64 (C.revision appended))
    (C.Note_revision.to_int64 receipt.selection.revision)
    (String.equal (read appended receipt.selection) "first line\r\nβé");
  let restored = round_trip appended in
  expect_replay restored create_identity create_request created;
  expect_replay restored append_identity append receipt;
  printf "create and append both replay after decoding\n";
  expect_mutation_error
    restored
    append_identity
    (C.Append { expected = created.selection; content = body "different" })
    C.Idempotency_conflict;
  expect_mutation_error
    restored
    (client "competing")
    append
    (C.Note_precondition (C.Stale_note_revision receipt.selection.revision));
  expect_read_error
    restored
    created.selection
    (C.Stale_note_revision receipt.selection.revision);
  let removed, deleted =
    applied restored (client "delete") (C.Delete receipt.selection)
  in
  expect_read_error removed deleted.selection C.Note_not_found;
  assert (String.equal (read initial created.selection) "first line");
  let recreated, next =
    applied removed (client "recreate") (create removed "review/work.md" "")
  in
  expect_read_error recreated receipt.selection C.Note_not_found;
  assert (not (C.Note_id.equal next.selection.id receipt.selection.id));
  printf "a deleted ID stays gone when the virtual path is reused\n";
  [%expect
    {|
    catalog revision 2, note revision 2, exact text true
    create and append both replay after decoding
    a deleted ID stays gone when the virtual path is reused
    |}]
;;

let%test_unit
    "an append/replace model checks bytes, revisions, fresh conflicts and immutability"
  =
  let start = empty () in
  let first, receipt = applied start (client "same-key") (create start "work" "") in
  let initial, independent =
    applied first (client ~principal:"pri_bob" "same-key") (create first "side" "fixed")
  in
  let final, _, _ =
    List.fold
      (List.range 0 30)
      ~init:(initial, receipt.selection, "")
      ~f:(fun (catalog, expected, model) index ->
        let content = if index mod 2 = 0 then "α" else "🙂\r\n" in
        let operation, next_model =
          if index mod 3 = 0
          then C.Replace { expected; content = body content }, content
          else C.Append { expected; content = body content }, model ^ content
        in
        let identity = client (Int.to_string index) in
        let updated, receipt = applied catalog identity operation in
        let updated = if index mod 5 = 0 then round_trip updated else updated in
        assert (String.equal (read catalog expected) model);
        assert (String.equal (read updated receipt.selection) next_model);
        assert (
          Int64.equal
            (C.Note_revision.to_int64 receipt.selection.revision)
            (Int64.of_int (index + 2)));
        assert (
          Int64.equal
            (C.Catalog_revision.to_int64 (C.revision updated))
            (Int64.of_int (index + 3)));
        expect_replay updated identity operation receipt;
        expect_mutation_error
          updated
          (client ("loser-" ^ Int.to_string index))
          operation
          (C.Note_precondition (C.Stale_note_revision receipt.selection.revision));
        updated, receipt.selection, next_model)
  in
  assert (String.equal (read final independent.selection) "fixed");
  assert (
    List.equal
      String.equal
      (List.map (C.list final) ~f:(fun metadata -> C.Note_path.to_string metadata.path))
      [ "side"; "work" ])
;;

let%test_unit "validated paths, counters and UTF-8 byte budgets" =
  let invalid_paths =
    [ ""
    ; "/absolute"
    ; "a/"
    ; "a//b"
    ; "."
    ; "a/.."
    ; "a/./b"
    ; "a\\b"
    ; "a:b"
    ; "uné"
    ; "a b"
    ; String.make 64 'a'
    ; String.concat ~sep:"/" (List.init 9 ~f:(fun _ -> "a"))
    ; String.concat ~sep:"/" (List.init 4 ~f:(fun _ -> String.make 63 'a'))
    ]
  in
  List.iter invalid_paths ~f:(fun value ->
    assert (Result.is_error (C.Note_path.of_string value)));
  assert (Result.is_error (C.Note_id.of_int64 0L));
  assert (Result.is_error (C.Note_revision.of_int64 0L));
  assert (Result.is_error (C.Catalog_revision.of_int64 (-1L)));
  List.iter
    [ "\000"; "\xc0\xaf"; "\xed\xa0\x80"; "\xf0\x9f\x99"; String.make 65537 'x' ]
    ~f:(fun value -> assert (Result.is_error (C.Content.of_string value)));
  let full = String.make 65532 'x' ^ "🙂" in
  let start = empty () in
  let catalog, receipt = applied start (client "full") (create start "max" full) in
  let append text = C.Append { expected = receipt.selection; content = body text } in
  expect_mutation_error catalog (client "too-large") (append "é") C.Note_byte_limit;
  expect_mutation_error catalog (client "empty") (append "") C.Empty_append;
  assert (String.equal (read catalog receipt.selection) full)
;;

let%test_unit "typed owner and identity inputs are revalidated before admission" =
  let malformed = Sexp.Atom "invalid" in
  let bad_owner =
    P.Session_ref.create
      ~server_id:(P.Id.Server.t_of_sexp malformed)
      ~session_id:(ok (P.Id.Session.of_string "ses_notes"))
  in
  assert (Result.is_error (C.empty ~owner:bad_owner));
  let bad_principal =
    C.Mutation_identity.Client
      { principal = P.Id.Principal.t_of_sexp malformed
      ; key = ok (P.Idempotency_key.of_string "key")
      }
  in
  let bad_key =
    C.Mutation_identity.Client
      { principal = ok (P.Id.Principal.of_string "pri_alice")
      ; key = P.Idempotency_key.t_of_sexp (Sexp.Atom "")
      }
  in
  let bad_invocation = C.Mutation_identity.Native (P.Id.Invocation.t_of_sexp malformed) in
  let catalog = empty () in
  let operation = create catalog "notes" "" in
  List.iter [ bad_principal; bad_key; bad_invocation ] ~f:(fun identity ->
    expect_mutation_error catalog identity operation C.Invalid_identity);
  let _, _ = applied catalog (client "key") operation in
  assert (List.is_empty (C.list catalog))
;;

let%test_unit "receipt replay binds operation, revision, identity, path and exact bytes" =
  let start = empty () in
  let identity = client "inv_same" in
  let original = create start "a" "é" in
  let first, a = applied start identity original in
  let second, b = applied first (native "inv_same") (create first "b" "") in
  let append_identity = client "append" in
  let edit = C.{ expected = a.selection; content = body "β" } in
  let updated, receipt = applied second append_identity (C.Append edit) in
  expect_replay updated identity original a;
  expect_replay updated append_identity (C.Append edit) receipt;
  List.iter
    [ create start "b" "é"; create start "a" "é"; create first "a" "é" ]
    ~f:(fun operation ->
      expect_mutation_error updated identity operation C.Idempotency_conflict);
  List.iter
    [ C.Replace edit
    ; C.Append { edit with expected = b.selection }
    ; C.Append { edit with expected = receipt.selection }
    ]
    ~f:(fun operation ->
      expect_mutation_error updated append_identity operation C.Idempotency_conflict)
;;

let%test_unit "live byte and entry caps release capacity after deletion" =
  let count = 32 in
  let text = String.make 65536 'x' in
  let full, last =
    List.fold
      (List.range 0 count)
      ~init:(empty (), None)
      ~f:(fun (catalog, _) index ->
        let key = Int.to_string index in
        let catalog, receipt =
          applied catalog (client key) (create catalog ("full-" ^ key) text)
        in
        catalog, Some receipt.selection)
  in
  expect_mutation_error
    full
    (client "overflow")
    (create full "overflow" "x")
    C.Catalog_byte_limit;
  let freed, _ = applied full (client "release") (C.Delete (Option.value_exn last)) in
  let _, _ = applied freed (client "overflow") (create freed "overflow" "x") in
  let full =
    List.fold (List.range 0 128) ~init:(empty ()) ~f:(fun catalog index ->
      let key = Int.to_string index in
      fst (applied catalog (client key) (create catalog key "")))
  in
  expect_mutation_error
    full
    (client "entry")
    (create full "extra" "")
    C.Catalog_entry_limit;
  expect_mutation_error
    full
    (client "existing")
    (create full "0" "")
    C.Path_already_exists;
  let first = List.hd_exn (C.list full) in
  let freed, _ = applied full (client "delete") (C.Delete first.selection) in
  let updated, _ = applied freed (client "entry") (create freed "extra" "") in
  assert (Int.equal (List.length (C.list updated)) 128)
;;

let replace_field name f = function
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         key, if String.equal name key then f value else value))
  | _ -> failwith ("expected object with " ^ name)
;;

let add_field name value = function
  | `Object fields -> `Object (fields @ [ name, value ])
  | _ -> failwith ("expected object for extension " ^ name)
;;

let get_field name json =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith ("missing field " ^ name)
;;

let first_item f = function
  | `Array (first :: rest) -> `Array (f first :: rest)
  | _ -> failwith "expected nonempty array"
;;

let items = function
  | `Array values -> values
  | _ -> failwith "expected array"
;;

let decode_json json =
  C.of_document (ok (D.Document.inspect ~limits:D.Limits.default json)) ~owner:session
;;

let json catalog = D.Document.json (ok (C.to_document catalog))

let%test_unit "optional fields survive decoding and mutation at stable record identities" =
  let start = empty () in
  let first, receipt = applied start (client "create") (create start "first" "a") in
  let extension = `Object [ "future", `Array [ `True; `Number "1.0" ] ] in
  let added name = add_field name extension in
  let with_extensions =
    json first
    |> added "future-envelope"
    |> replace_field "payload" (fun payload ->
      payload
      |> added "future-payload"
      |> replace_field "owner" (added "future-owner")
      |> replace_field "notes" (first_item (added "future-note"))
      |> replace_field
           "receipts"
           (first_item (replace_field "identity" (added "future-identity"))))
  in
  let restored = ok (decode_json with_extensions) in
  assert (D.Json.equal with_extensions (json restored));
  let second, _ = applied restored (client "second") (create restored "second" "b") in
  let updated, appended =
    applied
      second
      (client "append")
      (C.Append { expected = receipt.selection; content = body "é" })
  in
  let has json name = assert (D.Json.equal extension (get_field name json)) in
  let encoded = json updated in
  has encoded "future-envelope";
  let payload = get_field "payload" encoded in
  has payload "future-payload";
  has (get_field "owner" payload) "future-owner";
  let unchanged_id =
    `String (Int64.to_string (C.Note_id.to_int64 receipt.selection.id))
  in
  let original =
    items (get_field "notes" payload)
    |> List.find_exn ~f:(fun note -> D.Json.equal unchanged_id (get_field "id" note))
  in
  has original "future-note";
  let oldest = List.last_exn (items (get_field "receipts" payload)) in
  has (get_field "identity" oldest) "future-identity";
  (match C.apply updated ~identity:(client "delete") (C.Delete appended.selection) with
   | Error (Document_error (Extension_conflict _)) -> ()
   | Error _ | Ok _ -> failwith "delete lost optional note data");
  assert (String.equal (read updated appended.selection) "aé")
;;

let%test_unit "decoder rejects impossible receipt history and wrong ownership" =
  let start = empty () in
  let first, receipt = applied start (client "initial") (create start "first" "a") in
  let updated, _ =
    applied
      first
      (client "append")
      (C.Append { expected = receipt.selection; content = body "b" })
  in
  let encoded = json updated in
  let payload = get_field "payload" encoded in
  let document = ok (D.Document.inspect ~limits:D.Limits.default encoded) in
  assert (Result.is_error (C.of_document document ~owner:(owner "ses_other")));
  let corrupt name f =
    assert (
      Result.is_error
        (decode_json (replace_field "payload" (replace_field name f) encoded)))
  in
  corrupt "revision" (fun _ -> `String "02");
  corrupt "notes" (first_item (replace_field "revision" (fun _ -> `String "1")));
  corrupt
    "receipts"
    (first_item (replace_field "catalog_revision" (fun _ -> `String "1")));
  let receipts = items (get_field "receipts" payload) in
  let first_identity = get_field "identity" (List.last_exn receipts) in
  corrupt "receipts" (first_item (replace_field "identity" (fun _ -> first_identity)));
  assert (
    Result.is_error
      (decode_json
         (add_field "required_semantics" (`Array [ `String "future-note-rule" ]) encoded)))
;;

let%test_unit "receipt expiry rejects stale retries and counters stop at max int64" =
  let start = empty () in
  let create_identity = client "create" in
  let create_request = create start "bounded" "" in
  let first, receipt = applied start create_identity create_request in
  let saturated, selection =
    List.fold
      (List.range 0 512)
      ~init:(first, receipt.selection)
      ~f:(fun (catalog, expected) index ->
        let catalog, receipt =
          applied
            catalog
            (client (Int.to_string index))
            (C.Append { expected; content = body "x" })
        in
        catalog, receipt.selection)
  in
  let saturated = round_trip saturated in
  assert (String.equal (read saturated selection) (String.make 512 'x'));
  expect_mutation_error
    saturated
    create_identity
    create_request
    (C.Stale_catalog_revision (C.revision saturated));
  let first_append = C.Append { expected = receipt.selection; content = body "x" } in
  (match ok (C.apply saturated ~identity:(client "0") first_append) with
   | Replayed _ -> ()
   | Applied _ -> failwith "retained append lost its receipt");
  let removed, _ = applied saturated (client "remove") (C.Delete selection) in
  expect_mutation_error
    removed
    (client "0")
    first_append
    (C.Note_precondition C.Note_not_found);
  let recreated, newest =
    applied removed (client "recreate") (create removed "bounded" "new")
  in
  expect_mutation_error
    recreated
    (client "0")
    first_append
    (C.Note_precondition C.Note_not_found);
  assert (String.equal (read recreated newest.selection) "new");
  let add_at_eviction_boundary = function
    | `Array values ->
      `Array
        (List.rev
           (match List.rev values with
            | last :: rest -> add_field "future-receipt" `True last :: rest
            | [] -> failwith "empty receipt window"))
    | _ -> failwith "expected receipt window"
  in
  let retained =
    json saturated
    |> replace_field "payload" (replace_field "receipts" add_at_eviction_boundary)
    |> decode_json
    |> ok
  in
  (match C.apply retained ~identity:(client "remove") (C.Delete selection) with
   | Error (Document_error (Extension_conflict _)) -> ()
   | Error _ | Ok _ -> failwith "receipt eviction lost optional data");
  assert (String.equal (read retained selection) (String.make 512 'x'));
  let _, _ = applied saturated create_identity (create saturated "fresh" "") in
  let offset = Int64.(max_value - C.Catalog_revision.to_int64 (C.revision saturated)) in
  let shift = function
    | `String integer -> `String (Int64.to_string Int64.(of_string integer + offset))
    | _ -> failwith "expected string counter"
  in
  let shift_items f values = `Array (List.map (items values) ~f) in
  let at_max =
    json saturated
    |> replace_field "payload" (fun payload ->
      payload
      |> replace_field "revision" shift
      |> replace_field
           "notes"
           (shift_items (fun note ->
              note |> replace_field "id" shift |> replace_field "changed_at" shift))
      |> replace_field
           "receipts"
           (shift_items (fun receipt ->
              receipt
              |> replace_field "catalog_revision" shift
              |> replace_field "selection" (replace_field "id" shift))))
    |> decode_json
    |> ok
  in
  assert (Int64.equal (C.Catalog_revision.to_int64 (C.revision at_max)) Int64.max_value);
  expect_mutation_error
    at_max
    create_identity
    create_request
    (C.Stale_catalog_revision (C.revision at_max));
  expect_mutation_error
    at_max
    (client "new")
    (create at_max "extra" "")
    C.Revision_exhausted
;;
