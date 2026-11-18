open! Core
module R = Inference.Request
module D = Document_schema
module T = Request_tests

let guidance ~text ~provenance =
  R.Guidance.create ~text ~provenance ~limits:T.limits |> T.document_ok
;;

let%expect_test "guidance is exact bounded private text, never target or history" =
  let selected = T.target [] in
  let original =
    R.create ~target:selected ~history:[] ~tools:[] ~assets:[] ~limits:T.limits |> T.ok
  in
  let value = guidance ~text:"Untrusted guidance: \"完成\"\n" ~provenance:"snapshot:1" in
  let captured =
    R.with_guidance original ~guidance:(Some value) ~limits:T.limits |> T.ok
  in
  let independent =
    `Object
      [ "target", R.Target.to_json selected
      ; "history", `Array []
      ; "tools", `Array []
      ; "assets", `Array []
      ; ( "guidance"
        , `Object
            [ "role", `String "user"
            ; "text", `String "Untrusted guidance: \"完成\"\n"
            ; "provenance", `String "snapshot:1"
            ] )
      ]
  in
  let bytes = String.length (Jsonaf.to_string independent) in
  assert (Int.equal bytes (R.encoded_bytes captured));
  assert (
    Result.is_error
      (R.with_guidance original ~guidance:(Some value) ~limits:(T.bounded (bytes - 1))));
  assert (
    Result.is_ok
      (R.with_guidance original ~guidance:(Some value) ~limits:(T.bounded bytes)));
  let replaced =
    R.with_guidance
      captured
      ~guidance:(Some (guidance ~text:"replacement" ~provenance:"snapshot:2"))
      ~limits:T.limits
    |> T.ok
  in
  assert (
    String.equal (R.Guidance.text (Option.value_exn (R.guidance replaced))) "replacement");
  let cleared = R.with_guidance replaced ~guidance:None ~limits:T.limits |> T.ok in
  assert (Option.is_none (R.guidance original));
  assert (Option.is_none (R.guidance cleared));
  assert (Int.equal (R.encoded_bytes original) (R.encoded_bytes cleared));
  assert (R.Target.equal selected (R.target replaced));
  assert (List.is_empty (R.history replaced));
  print_endline
    "exact escaped aggregate bound; replace/clear without history or target edits";
  [%expect
    {| exact escaped aggregate bound; replace/clear without history or target edits |}]
;;

let%test_unit "guidance validates UTF8, private labels and fixed hard ceilings" =
  List.iter
    [ "", "source"
    ; "\255", "source"
    ; "text", "\255"
    ; "text", " \n"
    ; "text", String.make 1025 'p'
    ; String.make 524289 'x', "source"
    ]
    ~f:(fun (text, provenance) ->
      assert (Result.is_error (R.Guidance.create ~text ~provenance ~limits:T.limits)));
  let a = guidance ~text:"same bytes" ~provenance:"one" in
  let b = guidance ~text:"same bytes" ~provenance:"two" in
  assert (not (R.Guidance.equal a b));
  let bytes = String.length (Jsonaf.to_string (R.Guidance.to_json a)) in
  assert (
    Result.is_error
      (R.Guidance.create
         ~text:(R.Guidance.text a)
         ~provenance:(R.Guidance.provenance a)
         ~limits:(T.bounded (bytes - 1))))
;;
