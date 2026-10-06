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
           Unit.equal
           Error.equal
           (Json.validate ~limits:(limits (encoded_bytes - 1)) json)
           (Error (Error.Limit_exceeded "bytes"))));
  print_endline "300 exact serializer-boundary checks";
  [%expect {| 300 exact serializer-boundary checks |}]
;;
