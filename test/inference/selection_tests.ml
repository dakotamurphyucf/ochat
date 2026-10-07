open! Core
module S = Inference.Selection
module R = Inference.Request
module D = Document_schema

let limits = Transcript.Admission.default

let ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (R.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let document_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let target () =
  R.Target.create
    ~adapter:"synthetic"
    ~profile:"selected"
    ~profile_revision:None
    ~account:None
    ~endpoint:"local-inference"
    ~model:"model"
    ~settings:[]
    ~limits
  |> ok
;;

let object_fields = function
  | `Object fields -> fields
  | _ -> assert false
;;

let bounded ?(depth = 160) ?(fields = 1000000) ?(nodes = 2000000) bytes =
  D.Limits.create ~max_bytes:bytes ~max_depth:depth ~max_fields:fields ~max_nodes:nodes
  |> document_ok
;;

let%expect_test "unresolved capture preserves complete wrapper and target evidence" =
  let unknown = `Object [ "exact", `Number "1e+00"; "present_null", `Null ] in
  let raw =
    `Object
      [ "future_before", unknown
      ; "state", `String "unresolved"
      ; "future_after", `String "完成"
      ]
  in
  let original = S.of_json raw ~limits |> ok in
  assert (S.equal original (S.unresolved ~limits |> ok));
  assert (not (D.Json.equal raw (S.to_json (S.unresolved ~limits |> ok))));
  let target_json =
    `Object (("future_target", unknown) :: object_fields (R.Target.to_json (target ())))
  in
  let selected = R.Target.of_json target_json ~limits |> ok in
  let captured = S.capture original ~target:selected ~limits |> ok in
  let expected =
    `Object
      [ "future_before", unknown
      ; "state", `String "captured"
      ; "future_after", `String "完成"
      ; "target", target_json
      ]
  in
  assert (Jsonaf.exactly_equal expected (S.to_json captured));
  assert (Jsonaf.exactly_equal raw (S.to_json original));
  let restored = S.of_json (S.to_json captured) ~limits |> ok in
  assert (Jsonaf.exactly_equal expected (S.to_json restored));
  (match S.view restored with
   | Captured retained ->
     assert (Jsonaf.exactly_equal target_json (R.Target.to_json retained))
   | Unresolved -> assert false);
  print_endline
    "unknown wrapper and target retained; original unchanged; unresolved grants no target";
  [%expect
    {| unknown wrapper and target retained; original unchanged; unresolved grants no target |}]
;;

let%test_unit "repeated capture preserves original order and rejects changed evidence" =
  let fields =
    ("future", `Number "1e+00") :: object_fields (R.Target.to_json (target ()))
  in
  let selected = R.Target.of_json (`Object fields) ~limits |> ok in
  let raw =
    `Object
      [ "future_wrapper", `False
      ; "target", R.Target.to_json selected
      ; "state", `String "captured"
      ]
  in
  let original = S.of_json raw ~limits |> ok in
  let reordered = R.Target.of_json (`Object (List.rev fields)) ~limits |> ok in
  let repeated = S.capture original ~target:reordered ~limits |> ok in
  assert (Jsonaf.exactly_equal raw (S.to_json repeated));
  List.iter
    [ `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "future" then `Number "1.0" else value))
    ; `Object (("account", `Null) :: fields)
    ; `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "model" then `String "different" else value))
    ]
    ~f:(fun changed ->
      let changed = R.Target.of_json changed ~limits |> ok in
      assert (Result.is_error (S.capture original ~target:changed ~limits)));
  assert (Jsonaf.exactly_equal raw (S.to_json original))
;;

let%test_unit "selection admission rejects contradictory and malformed original JSON" =
  let target_json = R.Target.to_json (target ()) in
  List.iter
    [ `Null
    ; `Object []
    ; `Object [ "state", `String "unknown" ]
    ; `Object [ "state", `String "unresolved"; "target", `Null ]
    ; `Object [ "state", `String "unresolved"; "target", target_json ]
    ; `Object [ "state", `String "captured" ]
    ; `Object [ "state", `String "captured"; "target", `Null ]
    ; `Object [ "state", `String "captured"; "target", `Object [] ]
    ; `Object [ "state", `String "unresolved"; "state", `String "captured" ]
    ; `Object [ "state", `String "unresolved"; "future", `Number "01" ]
    ; `Object [ "state", `String "unresolved"; "future", `String "\255" ]
    ]
    ~f:(fun json -> assert (Result.is_error (S.of_json json ~limits)))
;;

let%test_unit
    "original bounds apply before capture and whole wrapper growth remains bounded"
  =
  let selected = target () in
  let nested = `Object [ "leaf", `String "value" ] in
  let original =
    S.of_json (`Object [ "state", `String "unresolved"; "future", nested ]) ~limits |> ok
  in
  let bytes = D.Json.validate_and_measure ~limits (S.to_json original) |> document_ok in
  List.iter
    [ bounded (bytes - 1)
    ; bounded ~depth:1 100000
    ; bounded ~fields:2 100000
    ; bounded ~nodes:3 100000
    ]
    ~f:(fun limits ->
      assert (Result.is_error (S.validate original ~limits));
      assert (Result.is_error (S.capture original ~target:selected ~limits)));
  let target_bytes =
    D.Json.validate_and_measure ~limits (R.Target.to_json selected) |> document_ok
  in
  let target_limits = bounded target_bytes in
  assert (Result.is_ok (R.Target.validate selected ~limits:target_limits));
  assert (Result.is_error (S.captured selected ~limits:target_limits));
  let captured = S.capture original ~target:selected ~limits |> ok in
  let full_bytes =
    D.Json.validate_and_measure ~limits (S.to_json captured) |> document_ok
  in
  assert (Result.is_ok (S.capture original ~target:selected ~limits:(bounded full_bytes)));
  assert (
    Result.is_error
      (S.capture original ~target:selected ~limits:(bounded (full_bytes - 1))))
;;
