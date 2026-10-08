open! Core
module R = Inference.Request
module D = Document_schema
module P = History_entry.Payload

let limits = Transcript.Admission.default

let document_ok result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (R.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let setting value =
  R.Setting.create ~name:"temperature" ~value ~provenance:Captured_prompt ~limits |> ok
;;

let target settings =
  R.Target.create
    ~adapter:"synthetic"
    ~profile:"selected"
    ~profile_revision:None
    ~account:(Some "host-account")
    ~endpoint:"https://example.test/inference"
    ~model:"arbitrary-model"
    ~settings
    ~limits
  |> ok
;;

let bounded bytes =
  D.Limits.create ~max_bytes:bytes ~max_depth:160 ~max_fields:1000000 ~max_nodes:2000000
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let%expect_test "target preserves setting absence, null and actual value" =
  List.iter
    [ P.Presence.Absent; Null; Value (`Number "1e+00") ]
    ~f:(fun value ->
      let original = target [ setting value ] in
      let restored = R.Target.of_json (R.Target.to_json original) ~limits |> ok in
      assert (R.Target.equal original restored);
      let actual = R.Setting.value (List.hd_exn (R.Target.settings restored)) in
      print_s
        [%sexp
          ((match actual with
            | Absent -> "absent"
            | Null -> "null"
            | Value value -> Jsonaf.to_string value)
           : string)]);
  [%expect
    {|
    absent
    null
    1e+00
    |}]
;;

let%expect_test "target preserves unknown JSON but member order is not identity" =
  let original = R.Target.to_json (target []) in
  let raw =
    match original with
    | `Object fields ->
      `Object
        (("future", `Object [ "exact", `Number "1e+00"; "unicode", `String "完成" ])
         :: fields)
    | _ -> assert false
  in
  let restored = R.Target.of_json raw ~limits |> ok in
  assert (Jsonaf.exactly_equal raw (R.Target.to_json restored));
  let reordered =
    match raw with
    | `Object fields -> `Object (List.rev fields)
    | _ -> assert false
  in
  assert (R.Target.equal restored (R.Target.of_json reordered ~limits |> ok));
  let exact = D.Json.validate_and_measure ~limits raw |> document_ok in
  assert (Result.is_error (R.Target.of_json raw ~limits:(bounded (exact - 1))));
  let duplicate =
    match raw with
    | `Object fields -> `Object (("model", `String "other") :: fields)
    | _ -> assert false
  in
  assert (Result.is_error (R.Target.of_json duplicate ~limits));
  print_endline
    "exact raw retained; reordered equal; whole bounds and duplicates enforced";
  [%expect
    {| exact raw retained; reordered equal; whole bounds and duplicates enforced |}]
;;

let%expect_test "JSON presence never silently normalizes native Value null" =
  let make_setting value =
    R.Setting.create ~name:"temperature" ~value ~provenance:Execution_override ~limits
  in
  let tool ~parameters ~output_schema =
    R.Tool_spec.create
      ~name:"inspect"
      ~description:Absent
      ~output_schema
      ~view:(Function { parameters; strict = Null })
      ~limits
  in
  assert (Result.is_error (make_setting (Value `Null)));
  assert (Result.is_ok (make_setting Null));
  assert (Result.is_error (tool ~parameters:(Value `Null) ~output_schema:Absent));
  assert (Result.is_error (tool ~parameters:Null ~output_schema:(Value `Null)));
  assert (Result.is_ok (tool ~parameters:Null ~output_schema:Null));
  print_endline "Value null rejected; explicit Null retained";
  [%expect {| Value null rejected; explicit Null retained |}]
;;

let%expect_test "native and restored setting JSON share malformed atom admission" =
  List.iter
    [ `Number "01"; `Number "NaN"; `String "\255" ]
    ~f:(fun value ->
      assert (
        Result.is_error
          (R.Setting.create
             ~name:"temperature"
             ~value:(Value value)
             ~provenance:Captured_prompt
             ~limits)));
  let duplicated = setting (Value (`Number "1")) in
  assert (
    Result.is_error
      (R.Target.create
         ~adapter:"synthetic"
         ~profile:"selected"
         ~profile_revision:None
         ~account:None
         ~endpoint:"loopback"
         ~model:"model"
         ~settings:[ duplicated; duplicated ]
         ~limits));
  print_endline "invalid numbers, UTF8 and duplicate settings rejected";
  [%expect {| invalid numbers, UTF8 and duplicate settings rejected |}]
;;

let%test_unit "request asset admission equals independent base64 representation" =
  let selected = target [] in
  let asset =
    R.Asset.create
      ~reference:"local-image"
      ~kind:Image
      ~media_type:"image/png"
      ~bytes:"f"
      ~max_bytes:1
    |> ok
  in
  let request =
    R.create ~target:selected ~history:[] ~tools:[] ~assets:[ asset ] ~limits |> ok
  in
  let independent =
    `Object
      [ "target", R.Target.to_json selected
      ; "history", `Array []
      ; "tools", `Array []
      ; ( "assets"
        , `Array
            [ `Object
                [ "reference", `String "local-image"
                ; "kind", `String "image"
                ; "media_type", `String "image/png"
                ; "data", `String "Zg=="
                ]
            ] )
      ]
  in
  let measured = D.Json.validate_and_measure ~limits independent |> document_ok in
  assert (R.encoded_bytes request = String.length (Jsonaf.to_string independent));
  assert (R.encoded_bytes request = measured);
  assert (
    Result.is_ok
      (R.create
         ~target:selected
         ~history:[]
         ~tools:[]
         ~assets:[ asset ]
         ~limits:(bounded measured)));
  assert (
    Result.is_error
      (R.create
         ~target:selected
         ~history:[]
         ~tools:[]
         ~assets:[ asset ]
         ~limits:(bounded (measured - 1))))
;;

let%test_unit "aggregate assets reject even when each body fits; bytes are binary" =
  let selected = target [] in
  let asset reference =
    R.Asset.create
      ~reference
      ~kind:(Document { filename = Some "example.bin" })
      ~media_type:"application/octet-stream"
      ~bytes:"\000\255\001"
      ~max_bytes:3
    |> ok
  in
  let a = asset "a"
  and b = asset "b" in
  assert (String.equal (R.Asset.bytes a) "\000\255\001");
  let full =
    R.create ~target:selected ~history:[] ~tools:[] ~assets:[ a; b ] ~limits |> ok
  in
  let ceiling = bounded (R.encoded_bytes full - 1) in
  assert (
    Result.is_ok
      (R.create ~target:selected ~history:[] ~tools:[] ~assets:[ a ] ~limits:ceiling));
  assert (
    Result.is_error
      (R.create ~target:selected ~history:[] ~tools:[] ~assets:[ a; b ] ~limits:ceiling));
  assert (
    Result.is_error
      (R.create ~target:selected ~history:[] ~tools:[] ~assets:[ a; a ] ~limits));
  assert (
    Result.is_error
      (R.Asset.create
         ~reference:"image"
         ~kind:Image
         ~media_type:"image/png"
         ~bytes:"xx"
         ~max_bytes:1))
;;

let%test_unit "neutral request retains exact host IDs and canonical payload unknowns" =
  let semantic =
    P.Semantic.create
      (Unknown { provider_kind = "synthetic.future" })
      ~metadata:P.Metadata.empty
    |> Result.ok_or_failwith
  in
  let raw = `Object [ "type", `String "synthetic.future"; "raw", `Number "1.00" ] in
  let origin =
    P.Origin.create
      ~adapter:"synthetic"
      ~provider:"selected"
      ~account:None
      ~endpoint:"https://example.test"
      ~profile:(Some "selected")
      ~model:(Some "arbitrary-model")
      ~replay_version:1
    |> Result.ok_or_failwith
  in
  let payload = P.captured semantic ~origin ~raw |> Result.ok_or_failwith in
  let id =
    History_entry.Id.create ~namespace:"host" ~sequence:7 |> Result.ok_or_failwith
  in
  let entry = History_entry.create_with_id ~id payload in
  let request =
    R.create ~target:(target []) ~history:[ entry ] ~tools:[] ~assets:[] ~limits |> ok
  in
  let retained = List.hd_exn (R.history request) in
  assert (History_entry.Id.equal id (History_entry.id retained));
  assert (
    Jsonaf.exactly_equal (P.to_json payload) (P.to_json (History_entry.payload retained)));
  assert (
    Result.is_error
      (R.create
         ~target:(target [])
         ~history:[ entry; entry ]
         ~tools:[]
         ~assets:[]
         ~limits))
;;

let%test_unit "ordinary descriptors do not fabricate host bindings" =
  let tool =
    R.Tool_spec.create
      ~name:"inspect"
      ~description:(Value "local")
      ~output_schema:Absent
      ~view:(Function { parameters = Value (`Object []); strict = Null })
      ~limits
    |> ok
  in
  let selected = target [] in
  assert (
    Result.is_ok
      (R.create ~target:selected ~history:[] ~tools:[ tool ] ~assets:[] ~limits));
  assert (
    Result.is_error
      (R.create ~target:selected ~history:[] ~tools:[ tool; tool ] ~assets:[] ~limits));
  assert (History_entry.Payload.Call_kind.equal (R.Tool_spec.kind tool) Function)
;;

let%expect_test
    "child model/settings updates preserve captured identity and unknown members"
  =
  let json = R.Target.to_json (target [ setting (Value (`Number "1")) ]) in
  let raw =
    match json with
    | `Object fields ->
      `Object
        (("target_future", `Object [ "literal", `Number "1e+00" ])
         :: List.map fields ~f:(fun (key, value) ->
           if String.equal key "settings"
           then
             ( key
             , `Array
                 [ (match value with
                    | `Array [ `Object setting_fields ] ->
                      `Object (("setting_future", `String "retained") :: setting_fields)
                    | _ -> assert false)
                 ] )
           else key, value))
    | _ -> assert false
  in
  let captured = R.Target.of_json raw ~limits |> ok in
  let updated =
    R.Target.with_model captured ~model:"child-model" ~limits
    |> ok
    |> fun target ->
    R.Target.with_setting
      target
      ~name:"temperature"
      ~value:Absent
      ~provenance:Execution_override
      ~limits
    |> ok
    |> fun target ->
    R.Target.with_setting
      target
      ~name:"max_output_tokens"
      ~value:(Value (`Number "32"))
      ~provenance:Execution_override
      ~limits
    |> ok
  in
  assert (String.equal (R.Target.model captured) "arbitrary-model");
  assert (String.equal (R.Target.model updated) "child-model");
  assert (String.equal (R.Target.adapter captured) (R.Target.adapter updated));
  assert (Option.equal String.equal (R.Target.account captured) (R.Target.account updated));
  assert (String.equal (R.Target.endpoint captured) (R.Target.endpoint updated));
  let actual = R.Target.to_json updated in
  assert (
    D.Json.equal
      (match D.Json.field raw ~name:"target_future" with
       | Value value -> value
       | _ -> assert false)
      (match D.Json.field actual ~name:"target_future" with
       | Value value -> value
       | _ -> assert false));
  let settings = R.Target.settings updated in
  assert (
    List.equal
      String.equal
      (List.map settings ~f:R.Setting.name)
      [ "temperature"; "max_output_tokens" ]);
  let first = R.Setting.to_json (List.hd_exn settings) in
  assert (
    D.Json.equal
      (match D.Json.field first ~name:"setting_future" with
       | Value value -> value
       | _ -> assert false)
      (`String "retained"));
  assert (
    match D.Json.field first ~name:"value" with
    | Absent -> true
    | Null | Value _ -> false);
  assert (R.Target.equal updated (R.Target.of_json actual ~limits |> ok));
  print_endline
    "identity inherited; model overridden; ordered settings and unknowns retained";
  [%expect
    {| identity inherited; model overridden; ordered settings and unknowns retained |}]
;;

let%test_unit
    "updating a value cannot repair failed original admission under tighter limits"
  =
  let original = setting (Value (`String (String.make 1000 'x'))) in
  let exact =
    D.Json.validate_and_measure ~limits (R.Setting.to_json original) |> document_ok
  in
  assert (
    Result.is_error
      (R.Setting.with_value
         original
         ~value:Absent
         ~provenance:Execution_override
         ~limits:(bounded (exact - 1))));
  assert (
    Result.is_error
      (R.Setting.with_value
         original
         ~value:(Value `Null)
         ~provenance:Execution_override
         ~limits));
  let captured = target [ original ] in
  let exact =
    D.Json.validate_and_measure ~limits (R.Target.to_json captured) |> document_ok
  in
  assert (
    Result.is_error
      (R.Target.with_model captured ~model:"x" ~limits:(bounded (exact - 1))));
  assert (
    Result.is_error
      (R.Target.with_setting
         captured
         ~name:"temperature"
         ~value:Absent
         ~provenance:Execution_override
         ~limits:(bounded (exact - 1))))
;;
