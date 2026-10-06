open! Core
module Schema = Document_schema
module Record = Agent_store.Document_record
module Wire = Openai.Responses.Codec.Wire

let ok result =
  Result.ok_or_failwith (Result.map_error result ~f:(fun _ -> "unexpected error"))
;;

let limits = Schema.Limits.default

let origin =
  ok
    (Wire.Origin.create
       ~provider:"test"
       ~account:(Some "account")
       ~endpoint:"https://example.invalid/v1/responses")
;;

type evidence =
  { label : string
  ; item : Wire.Item.t
  }

let codec =
  let shape =
    ok (Schema.Shape.object_ [ "label", Schema.Shape.value; "item", Schema.Shape.value ])
  in
  let decode json =
    match Schema.Json.field json ~name:"label", Schema.Json.field json ~name:"item" with
    | Value (`String label), Value item ->
      Result.map (Wire.Item.decode item ~origin) ~f:(fun item -> { label; item })
      |> Result.map_error ~f:(fun _ ->
        Schema.Error.Invalid_field { path = [ "item" ]; reason = "invalid wire item" })
    | _ ->
      Error (Schema.Error.Invalid_field { path = []; reason = "missing evidence fields" })
  in
  let encode { label; item } =
    Ok (`Object [ "label", `String label; "item", Wire.Item.raw item ])
  in
  ok
    (Schema.Domain_codec.create
       ~limits
       ~kind:"test.responses-evidence"
       ~version:2
       ~shape
       ~supported_semantics:[]
       ~decode
       ~encode)
;;

let%expect_test "wire capture survives storage conversion, host edit and reframing" =
  (* Independent stored JSON, including insignificant whitespace, models an
     earlier supported named-field version, not an old OCaml representation. *)
  let stored =
    {| {
    "format":"ochat.document", "schema_version":1,
    "kind":"test.responses-evidence", "future_envelope":null,
    "payload":{
      "heading":"before",
      "item":{"type":"function_call","id":"item-1","call_id":"call-1",
        "name":"local_tool","arguments":"{ \"amount\": 1.00 }",
        "caller":null,"future_provider":{"opaque":"unchanged","nullable":null}},
      "future_host":{"entry_id":"host-entry-7","nullable":null}
    }
  } |}
  in
  let frame =
    ok
      (Agent_store.Frame.encode
         ~max_payload_length:(Schema.Limits.max_bytes limits)
         ~flags:0
         stored)
  in
  let digest = Record.digest stored in
  let record = ok (Record.decode_file ~limits ~expected_digest:(Some digest) frame) in
  let conversion =
    let step =
      ok
        (Schema.Conversion.Step.create
           ~kind:"test.responses-evidence"
           ~from_version:1
           ~operations:[ Rename { parent = []; src = "heading"; dst = "label" } ])
    in
    ok
      (Schema.Conversion.create
         ~limits
         ~targets:[ "test.responses-evidence", 2 ]
         ~max_steps:2
         ~max_operations:4
         ~steps:[ step ])
  in
  let document = ok (Record.upgrade record ~conversion) in
  let carrier = ok (Schema.Domain_codec.decode codec document) in
  let evidence = Schema.Extension_carrier.value carrier in
  let edited =
    Schema.Extension_carrier.with_value carrier { evidence with label = "after" }
  in
  let encoded = ok (Schema.Domain_codec.encode codec edited) in
  let roundtrip =
    ok
      (Record.decode_file
         ~limits
         ~expected_digest:None
         (ok (Record.encode encoded ~limits ~flags:0)))
  in
  let reread =
    Schema.Extension_carrier.value
      (ok (Schema.Domain_codec.decode codec (Record.document roundtrip)))
  in
  let payload = Schema.Document.payload (Record.document roundtrip) in
  let field json name =
    match Schema.Json.field json ~name with
    | Absent -> "absent"
    | Null -> "null"
    | Value value -> Jsonaf.to_string value
  in
  print_s
    [%sexp
      (reread.label : string)
    , (String.equal
         (Jsonaf.to_string (Wire.Item.raw evidence.item))
         (Jsonaf.to_string (Wire.Item.raw reread.item))
       : bool)
    , (String.equal (Record.stored_bytes record) stored : bool)
    , (String.equal (Record.stored_digest record) digest : bool)
    , (not (String.equal (Record.stored_digest roundtrip) digest) : bool)];
  print_endline (field payload "heading");
  print_endline (field payload "future_host");
  print_endline (field (Schema.Document.json encoded) "future_envelope");
  (match Wire.Item.view reread.item with
   | Call (Function { call_id; arguments; _ }) ->
     print_s [%sexp (call_id : string), (arguments : string)]
   | _ -> failwith "function capture changed");
  [%expect
    {| 
    (after true true true true)
    absent
    {"entry_id":"host-entry-7","nullable":null}
    null
    (call-1 "{ \"amount\": 1.00 }")
  |}]
;;
