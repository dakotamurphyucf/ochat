open! Core
open Expect_test_helpers_core
open Document_schema

let%expect_test "encoded byte accounting matches the independent JSON serializer" =
  let unicode =
    Quickcheck.Generator.map String.Utf8.quickcheck_generator ~f:String.Utf8.to_string
  in
  Quickcheck.test
    ~trials:300
    (Quickcheck.Generator.tuple3 unicode unicode Int.quickcheck_generator)
    ~f:(fun (key, value, number) ->
      let json =
        `Object
          [ ( key
            , `Array
                [ `String value
                ; `Number (Int.to_string number)
                ; `Null
                ; `True
                ; `False
                ; `Object [ "\000\t\"\\", `String (key ^ "\000\n") ]
                ] )
          ]
      in
      let encoded_bytes = String.length (Jsonaf.to_string json) in
      let limits max_bytes =
        Limits.create ~max_bytes ~max_depth:8 ~max_fields:20 ~max_nodes:100
        |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Error.sexp_of_t error))
        |> Result.ok_or_failwith
      in
      require [%here] (Result.is_ok (Json.validate ~limits:(limits encoded_bytes) json));
      require
        [%here]
        (Result.equal
           Int.equal
           Error.equal
           (Json.validate_and_measure ~limits:(limits encoded_bytes) json)
           (Ok encoded_bytes));
      require
        [%here]
        (Result.equal
           Unit.equal
           Error.equal
           (Json.validate ~limits:(limits (encoded_bytes - 1)) json)
           (Json.validate_and_measure ~limits:(limits (encoded_bytes - 1)) json
            |> Result.map ~f:ignore));
      require
        [%here]
        (Result.equal
           Unit.equal
           Error.equal
           (Json.validate ~limits:(limits (encoded_bytes - 1)) json)
           (Error (Error.Limit_exceeded "bytes"))));
  print_endline "300 exact serializer-boundary checks";
  [%expect {| 300 exact serializer-boundary checks |}]
;;

let%expect_test "document decode reuses only the validation for its exact limits" =
  let json =
    `Object
      [ "format", `String "ochat.document"
      ; "kind", `String "example"
      ; "schema_version", `Number "1"
      ; "payload", `Object [ "future", `Array [ `String "\000\n\"\\"; `Null ] ]
      ]
  in
  let bytes = Jsonaf.to_string json in
  let limits max_bytes =
    Limits.create ~max_bytes ~max_depth:8 ~max_fields:20 ~max_nodes:100
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let exact = limits (String.length bytes) in
  let document =
    Document.decode ~limits:exact bytes
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  assert (Result.is_ok (Document.validate document ~limits:exact));
  let looser =
    Limits.create
      ~max_bytes:(String.length bytes + 1)
      ~max_depth:16
      ~max_fields:40
      ~max_nodes:200
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  assert (Result.is_ok (Document.validate document ~limits:looser));
  List.iter [ "bytes"; "depth"; "fields"; "nodes" ] ~f:(fun component ->
    let stricter =
      Limits.create
        ~max_bytes:(if String.equal component "bytes" then 1 else String.length bytes)
        ~max_depth:(if String.equal component "depth" then 1 else 8)
        ~max_fields:(if String.equal component "fields" then 1 else 20)
        ~max_nodes:(if String.equal component "nodes" then 1 else 100)
      |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    let rechecked = Document.validate document ~limits:stricter in
    assert (Result.is_error rechecked);
    assert (
      Result.equal Unit.equal Error.equal rechecked (Json.validate ~limits:stricter json)));
  assert (Result.is_ok (Document.inspect ~limits:exact json));
  let tight = limits (String.length bytes - 1) in
  assert (Result.is_error (Document.decode ~limits:tight bytes));
  assert (Result.is_error (Document.inspect ~limits:tight json));
  let duplicate =
    {|{"format":"ochat.document","kind":"example","schema_version":1,"payload":{"future":1,"future":2}}|}
  in
  assert (Result.is_error (Document.decode ~limits:Limits.default duplicate));
  print_endline "exact limits accept; tighter limits and duplicate unknown fields reject";
  [%expect {| exact limits accept; tighter limits and duplicate unknown fields reject |}]
;;

let%expect_test "ASCII proof retains high-byte validation and byte-error precedence" =
  let limits max_bytes =
    Limits.create ~max_bytes ~max_depth:8 ~max_fields:20 ~max_nodes:100
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let ascii = String.init 128 ~f:Char.of_int_exn in
  List.iter
    [ `String ascii; `Object [ ascii, `String ascii ] ]
    ~f:(fun json ->
      let bytes = String.length (Jsonaf.to_string json) in
      require [%here] (Result.is_ok (Json.validate ~limits:(limits bytes) json));
      require
        [%here]
        (Result.equal
           Unit.equal
           Error.equal
           (Json.validate ~limits:(limits (bytes - 1)) json)
           (Error (Error.Limit_exceeded "bytes"))));
  let malformed = String.of_char (Char.of_int_exn 255) in
  let string = `String malformed
  and key = `Object [ malformed, `Null ] in
  List.iter
    [ string, 2; key, 5 ]
    ~f:(fun (json, max_bytes) ->
      require
        [%here]
        (Result.equal
           Unit.equal
           Error.equal
           (Json.validate ~limits:(limits max_bytes) json)
           (Error (Error.Limit_exceeded "bytes"))));
  require
    [%here]
    (Result.equal
       Unit.equal
       Error.equal
       (Json.validate ~limits:(limits 3) string)
       (Error (Error.Invalid_field { path = []; reason = "invalid UTF-8 string" })));
  require
    [%here]
    (Result.equal
       Unit.equal
       Error.equal
       (Json.validate ~limits:(limits 6) key)
       (Error
          (Error.Invalid_field
             { path = [ malformed ]; reason = "invalid UTF-8 object key" })));
  print_endline "all ASCII admits; malformed high bytes preserve byte-first rejection";
  [%expect {| all ASCII admits; malformed high bytes preserve byte-first rejection |}]
;;
