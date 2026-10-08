open! Core

let%expect_test
    "standalone scope admission preserves actual parent and bounds unknown fields"
  =
  let limits = Transcript.Admission.default in
  let decode text = Transcript.Scope.of_json (Jsonaf.of_string text) ~limits in
  let nested =
    decode
      {|{"key":{"source":"child","attempt":"a2"},"parent":{"scope":{"source":"root","attempt":"a1"},"call_alias":"invoke","call_entry_id":null}}|}
    |> Result.ok_or_failwith
  in
  let root =
    decode {|{"key":{"source":"child","attempt":"a2"},"parent":null}|}
    |> Result.ok_or_failwith
  in
  let restored =
    Transcript.Scope.of_json (Transcript.Scope.to_json nested) ~limits
    |> Result.ok_or_failwith
  in
  print_s
    [%sexp
      (Transcript.Scope.equal nested restored : bool)
    , (Transcript.Scope.Key.equal
         (Transcript.Scope.key nested)
         (Transcript.Scope.key root)
       : bool)
    , (Transcript.Scope.equal nested root : bool)];
  let invalid =
    [ {|{"key":{"source":"child","attempt":"a2"},"parent":{"scope":{"source":"child","attempt":"a2"}}}|}
    ; {|{"key":{"source":"child","source":"root","attempt":"a2"},"parent":null}|}
    ]
  in
  print_s
    [%sexp (List.map invalid ~f:(fun text -> Result.is_error (decode text)) : bool list)];
  let small =
    Transcript.Admission.limits ~max_bytes:100
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let oversized =
    `Object
      [ "key", `Object [ "source", `String "child"; "attempt", `String "a2" ]
      ; "parent", `Null
      ; "future", `String (String.make 101 'x')
      ]
  in
  print_s
    [%sexp (Result.is_error (Transcript.Scope.of_json oversized ~limits:small) : bool)];
  [%expect
    {|
    (true true false)
    (true true)
    true |}]
;;
