open! Core
open Document_schema

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Error.t)]
;;

let limits = Limits.default

let shape =
  Shape.object_ [ "value", Shape.nullable (Shape.object_ [ "text", Shape.value ] |> ok) ]
  |> ok
;;

let invalid reason = Error (Error.Invalid_field { path = [ "value" ]; reason })

let validate = function
  | Some "" -> invalid "empty native value is invalid"
  | None | Some _ -> Ok ()
;;

let decode json =
  match Json.field json ~name:"value" with
  | Null -> Ok None
  | Value object_ ->
    (match Json.field object_ ~name:"text" with
     | Value (`String text) -> Result.map (validate (Some text)) ~f:(fun () -> Some text)
     | Absent | Null | Value _ -> invalid "text is required")
  | Absent -> invalid "value is required"
;;

let encode value =
  let value =
    match value with
    | None | Some "" -> `Null
    | Some text -> `Object [ "text", `String text ]
  in
  Ok (`Object [ "value", value ])
;;

let make_codec ?(limits = limits) ~encode () =
  Domain_codec.create_validated
    ~limits
    ~kind:"validated.example"
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~validate
    ~decode
    ~encode
  |> ok
;;

let%expect_test "original validation rejects normalization before invoking encoder" =
  let encodes = ref 0 in
  let codec =
    make_codec
      ~encode:(fun value ->
        incr encodes;
        encode value)
      ()
  in
  assert (
    Result.is_error
      (Domain_codec.encode codec (Extension_carrier.of_authored_value (Some ""))));
  assert (!encodes = 0);
  List.iter [ None; Some "retained" ] ~f:(fun original ->
    let document =
      Domain_codec.encode codec (Extension_carrier.of_authored_value original) |> ok
    in
    let restored = Domain_codec.decode codec document |> ok |> Extension_carrier.value in
    assert (Option.equal String.equal original restored));
  assert (!encodes = 2);
  print_endline "invalid original never encoded; valid originals roundtrip";
  [%expect {| invalid original never encoded; valid originals roundtrip |}]
;;

let%expect_test "validated serializers still enforce JSON shape bounds and preservation" =
  let writer_limits =
    Limits.create ~max_bytes:128 ~max_depth:8 ~max_fields:20 ~max_nodes:100 |> ok
  in
  List.iter
    [ `Object [ "value", `Number "invalid" ]
    ; `Array []
    ; `Object [ "value", `Null; "unexpected", `True ]
    ; `Object [ "value", `Object [ "text", `String (String.make 256 'a') ] ]
    ]
    ~f:(fun json ->
      let codec = make_codec ~limits:writer_limits ~encode:(fun _ -> Ok json) () in
      assert (
        Result.is_error
          (Domain_codec.encode codec (Extension_carrier.of_authored_value None))));
  let codec = make_codec ~encode () in
  let document =
    Document.decode
      ~limits
      {|{"format":"ochat.document","kind":"validated.example","schema_version":1,"payload":{"value":{"text":"before","future":null}},"future_envelope":true}|}
    |> ok
  in
  let restored = Domain_codec.decode codec document |> ok in
  assert (
    Result.is_error
      (Domain_codec.encode codec (Extension_carrier.with_value restored None)));
  let edited =
    Domain_codec.encode codec (Extension_carrier.with_value restored (Some "after")) |> ok
  in
  assert (String.is_substring (Document.to_string edited) ~substring:"future");
  assert (
    Option.equal
      String.equal
      (Domain_codec.decode codec edited |> ok |> Extension_carrier.value)
      (Some "after"));
  let tiny =
    Limits.create ~max_bytes:10 ~max_depth:8 ~max_fields:20 ~max_nodes:100 |> ok
  in
  let bounded = make_codec ~limits:tiny ~encode () in
  assert (
    Result.is_error
      (Domain_codec.encode bounded (Extension_carrier.of_authored_value None)));
  print_endline
    "malformed JSON, wrong shape, extra fields, bounds and unknown deletion reject";
  [%expect
    {| malformed JSON, wrong shape, extra fields, bounds and unknown deletion reject |}]
;;
