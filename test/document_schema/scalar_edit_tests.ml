open! Core
open Document_schema

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Error.t)]
;;

let limits = Limits.default

let original =
  Document.inspect
    ~limits
    (`Object
        [ "future_envelope", `Number "7e0"
        ; "kind", `String "scalar.example"
        ; "format", `String Document.format
        ; "schema_version", `Number "1"
        ; "required_semantics", `Array [ `String "future" ]
        ; "extensions", `Object [ "opaque", `Null ]
        ; ( "payload"
          , `Object
              [ ( "metadata"
                , `Object
                    [ "counter", `String "999"
                    ; "future_child", `Object [ "value", `Number "2.0" ]
                    ] )
              ; "text", `String "\000\n\"\\💡"
              ; "flag", `False
              ; "nullable", `Null
              ; "number", `Number "123456"
              ; "items", `Array [ `String "value" ]
              ] )
        ])
  |> ok
;;

(* Independently patch the original envelope, then use ordinary full admission. *)
let reference document ~limits updates =
  let rec set json path value =
    match json, path with
    | _, [] -> value
    | `Object fields, name :: rest ->
      `Object
        (List.map fields ~f:(fun (key, previous) ->
           key, if String.equal key name then set previous rest value else previous))
    | _ -> assert false
  in
  List.fold updates ~init:(Document.json document) ~f:(fun json (path, value) ->
    set json ("payload" :: path) value)
  |> Document.inspect ~limits
;;

let assert_reference document ~limits updates =
  let expected = reference document ~limits updates |> ok in
  let actual = Document.replace_payload_scalars document ~limits ~updates |> ok in
  assert (String.equal (Document.to_string actual) (Document.to_string expected));
  assert (
    String.equal
      (Jsonaf.to_string (Document.payload actual))
      (Jsonaf.to_string (Document.payload expected)));
  assert (String.equal (Document.kind actual) (Document.kind document));
  assert (Document.version actual = Document.version document);
  assert (
    List.equal
      String.equal
      (Document.required_semantics actual)
      (Document.required_semantics document));
  Json.validate ~limits (Document.json actual) |> ok
;;

let%expect_test "scalar edits match complete admission and refresh carried metadata" =
  let before = Document.to_string original in
  List.iter
    [ []
    ; [ [ "text" ], `String "aa"
      ; [ "metadata"; "counter" ], `String "998"
      ; [ "nullable" ], `True
      ; [ "flag" ], `Null
      ; [ "number" ], `Number "7e0"
      ]
    ; [ [ "text" ], `String "\n"
      ; [ "text" ], `String "aa"
      ; [ "nullable" ], `False
      ; [ "flag" ], `True
      ]
    ; [ [ "text" ], `String "\000"
      ; [ "text" ], `String "aaaa"
      ; [ "text" ], `String "💡"
      ; [ "text" ], `String "\"\\"
      ]
    ; [ [ "metadata"; "counter" ], `String "10000"
      ; [ "metadata"; "counter" ], `String "7"
      ]
    ]
    ~f:(assert_reference original ~limits);
  assert (String.equal before (Document.to_string original));
  print_endline "escaped sizes, scalar kinds and ordered edits preserve exact envelope";
  [%expect {| escaped sizes, scalar kinds and ordered edits preserve exact envelope |}]
;;

let%expect_test "scalar edits admit the original under every requested bound first" =
  let json = Document.json original in
  let profiles =
    [ "bytes", String.length (Document.to_string original) - 1, 128, 100_000, 1_000_000
    ; "depth", 16384, 2, 100_000, 1_000_000
    ; "fields", 16384, 128, 1, 1_000_000
    ; "nodes", 16384, 128, 100_000, 1
    ]
  in
  List.iter profiles ~f:(fun (dimension, max_bytes, max_depth, max_fields, max_nodes) ->
    let limits = Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes |> ok in
    let expected = Json.validate ~limits json in
    assert (
      match expected with
      | Error (Limit_exceeded actual) -> String.equal dimension actual
      | Error _ | Ok () -> false);
    List.iter
      [ []; [ [ "text" ], `Null ]; [ [ "text" ], `Number "01" ] ]
      ~f:(fun updates ->
        let actual =
          Document.replace_payload_scalars original ~limits ~updates
          |> Result.map ~f:(fun _ -> ())
        in
        assert (Result.equal Unit.equal Error.equal expected actual)));
  print_endline "stricter original bytes, depth, fields and nodes reject before edits";
  [%expect {| stricter original bytes, depth, fields and nodes reject before edits |}]
;;

let%expect_test "scalar edits reject malformed atoms and absent or container paths" =
  List.iter
    [ `Number "01"; `Number "NaN"; `Number "1e"; `String "\255" ]
    ~f:(fun value ->
      assert (
        Result.is_error
          (Document.replace_payload_scalars
             original
             ~limits
             ~updates:[ [ "text" ], value ])));
  List.iter
    [ [], `Null
    ; [ "absent" ], `Null
    ; [ "absent"; "child" ], `Null
    ; [ "text"; "child" ], `Null
    ; [ "items"; "0" ], `Null
    ; [ "items" ], `Null
    ; [ "metadata" ], `Null
    ; [ "text" ], `Object []
    ; [ "text" ], `Array []
    ]
    ~f:(fun update ->
      assert (
        match Document.replace_payload_scalars original ~limits ~updates:[ update ] with
        | Error (Invalid_field _) -> true
        | Error _ | Ok _ -> false));
  print_endline "invalid UTF8/numbers and non-scalar or missing paths reject";
  [%expect {| invalid UTF8/numbers and non-scalar or missing paths reject |}]
;;

let%expect_test "scalar growth retains complete envelope byte enforcement" =
  let tight =
    Limits.create
      ~max_bytes:(String.length (Document.to_string original))
      ~max_depth:128
      ~max_fields:100_000
      ~max_nodes:1_000_000
    |> ok
  in
  let growing = `String (String.make 100 'x') in
  Json.validate ~limits:tight growing |> ok;
  let edits = [ [ "metadata"; "counter" ], growing ] in
  assert (
    Result.equal
      Unit.equal
      Error.equal
      (reference original ~limits:tight edits |> Result.map ~f:(fun _ -> ()))
      (Document.replace_payload_scalars original ~limits:tight ~updates:edits
       |> Result.map ~f:(fun _ -> ())));
  assert (
    match Document.replace_payload_scalars original ~limits:tight ~updates:edits with
    | Error (Limit_exceeded "bytes") -> true
    | Error _ | Ok _ -> false);
  assert_reference
    original
    ~limits:tight
    [ [ "metadata"; "counter" ], growing; [ "metadata"; "counter" ], `String "7" ];
  print_endline
    "growing scalar fits alone but oversized envelope rejects; final shrink admits";
  [%expect
    {| growing scalar fits alone but oversized envelope rejects; final shrink admits |}]
;;
