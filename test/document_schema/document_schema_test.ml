open! Core
open Expect_test_helpers_core
open Document_schema

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Error.t)]
;;

let limits = Limits.default
let json text = Json.decode ~limits text |> ok
let document text = Document.decode ~limits text |> ok

let print_error = function
  | Ok _ -> print_endline "ok"
  | Error error -> print_s [%sexp (error : Error.t)]
;;

let print_json value = print_endline (Jsonaf.to_string value)

let print_document result =
  match result with
  | Ok doc -> print_endline (Document.to_string doc)
  | Error error -> print_s [%sexp (error : Error.t)]
;;

let step version operations =
  Conversion.Step.create ~kind:"example" ~from_version:version ~operations |> ok
;;

let registry ?(max_steps = 10) ?(max_operations = 10) ?(limits = limits) target steps =
  Conversion.create
    ~limits
    ~targets:[ "example", target ]
    ~max_steps
    ~max_operations
    ~steps
  |> ok
;;

let shape fields =
  Shape.object_ (List.map fields ~f:(fun name -> name, Shape.value)) |> ok
;;

let identity_codec ?(kind = "example") ?(version = 1) ?(semantics = []) shape =
  Domain_codec.create
    ~limits
    ~kind
    ~version
    ~shape
    ~supported_semantics:semantics
    ~decode:(fun json -> Ok json)
    ~encode:(fun json -> Ok json)
  |> ok
;;

let%expect_test
    "constructed two-version conversion: rename, structural move, missing default, null, \
     IDs and order"
  =
  let original =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"old_name":"demo","id":"host-9","order":["b","a"],"option":null,"settings":{"legacy":7,"unknown":{"deep":null}},"extra":true},"extensions":{"future":false},"future_envelope":[1,2]}|}
  in
  let conversion =
    registry
      3
      [ step
          1
          [ Rename { parent = []; src = "old_name"; dst = "name" }
          ; Default { path = [ "option" ]; value = `String "default" }
          ; Default { path = [ "missing" ]; value = `Number "4" }
          ]
      ; step
          2
          [ Default { path = [ "configuration" ]; value = `Object [] }
          ; Move { src = [ "settings"; "legacy" ]; dst = [ "configuration"; "count" ] }
          ]
      ]
  in
  let upgraded = Conversion.upgrade conversion original |> ok in
  print_json (Document.payload upgraded);
  print_endline (Document.to_string upgraded);
  print_s
    [%sexp
      (String.equal
         (Document.to_string upgraded)
         (Document.to_string (Conversion.upgrade conversion original |> ok))
       : bool)];
  print_s
    [%sexp
      (String.equal
         (Document.to_string upgraded)
         (Document.to_string (Conversion.upgrade conversion upgraded |> ok))
       : bool)];
  [%expect
    {|
    {"id":"host-9","order":["b","a"],"option":null,"settings":{"unknown":{"deep":null}},"extra":true,"name":"demo","missing":4,"configuration":{"count":7}}
    {"format":"ochat.document","schema_version":3,"kind":"example","payload":{"id":"host-9","order":["b","a"],"option":null,"settings":{"unknown":{"deep":null}},"extra":true,"name":"demo","missing":4,"configuration":{"count":7}},"extensions":{"future":false},"future_envelope":[1,2]}
    true
    true
    |}]
;;

let%expect_test "structural function conversion runs before current domain decoder" =
  let conversion =
    registry
      2
      [ Conversion.Step.of_function ~kind:"example" ~from_version:1 ~f:(fun payload ->
          match Json.field payload ~name:"ordered" with
          | Value (`Array items) ->
            Ok
              (`Object
                  [ ( "items"
                    , `Array (List.map items ~f:(fun value -> `Object [ "value", value ]))
                    )
                  ])
          | Absent | Null | Value _ ->
            Error
              (Error.Invalid_field { path = [ "ordered" ]; reason = "required array" }))
        |> ok
      ]
  in
  let current =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"ordered":["a","b"]}}|}
    |> Conversion.upgrade conversion
    |> ok
  in
  print_document (Ok current);
  let codec = identity_codec ~version:2 (shape [ "items" ]) in
  print_error (Domain_codec.decode codec current);
  [%expect
    {|
    {"format":"ochat.document","schema_version":2,"kind":"example","payload":{"items":[{"value":"a"},{"value":"b"}]}}
    ok
    |}]
;;

let%expect_test "presence is distinct and null is not defaulted" =
  let payload = json {|{"explicit":null,"value":false}|} in
  List.iter [ "missing"; "explicit"; "value" ] ~f:(fun name ->
    match Json.field payload ~name with
    | Absent -> print_endline "absent"
    | Null -> print_endline "null"
    | Value value -> print_json value);
  let codec = identity_codec (shape [ "explicit"; "value" ]) in
  let doc = Document.create ~limits ~kind:"example" ~version:1 ~payload |> ok in
  let restored = Domain_codec.decode codec doc |> ok in
  print_document (Domain_codec.encode codec restored);
  [%expect
    {|
    absent
    null
    false
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"explicit":null,"value":false}}
    |}]
;;

let%expect_test "nested unknown data survives domain edits at its original paths" =
  let nested = Shape.object_ [ "text", Shape.value ] |> ok in
  let root = Shape.object_ [ "id", Shape.value; "nested", nested ] |> ok in
  let codec = identity_codec root in
  let original =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"unknown_root":null,"id":"x","nested":{"unknown_nested":{"n":[1,null]},"text":"before"}},"extensions":{"future":42},"unknown_envelope":"preserved"}|}
  in
  let carrier = Domain_codec.decode codec original |> ok in
  print_json (Extension_carrier.value carrier);
  let edited =
    Extension_carrier.with_value carrier (json {|{"id":"x","nested":{"text":"after"}}|})
  in
  print_document (Domain_codec.encode codec edited);
  print_document
    (Domain_codec.encode
       codec
       (Extension_carrier.of_authored_value
          (json {|{"id":"new","nested":{"text":"authored"}}|})));
  print_error
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value carrier (json {|{"id":"x"}|})));
  [%expect
    {|
    {"id":"x","nested":{"text":"before"}}
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"unknown_root":null,"id":"x","nested":{"unknown_nested":{"n":[1,null]},"text":"after"}},"extensions":{"future":42},"unknown_envelope":"preserved"}
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"id":"new","nested":{"text":"authored"}}}
    (Extension_conflict (payload nested))
    |}]
;;

let%expect_test "unknown ownership promotion and authored collisions fail explicitly" =
  let original =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"known":1,"future":null}}|}
  in
  let old_codec = identity_codec (shape [ "known" ]) in
  let carrier = Domain_codec.decode old_codec original |> ok in
  let new_codec = identity_codec (shape [ "known"; "future" ]) in
  print_error
    (Domain_codec.encode
       new_codec
       (Extension_carrier.with_value carrier (json {|{"known":2,"future":null}|})));
  print_error (Domain_codec.encode new_codec carrier);
  print_error
    (Domain_codec.encode
       old_codec
       (Extension_carrier.with_value carrier (json {|{"known":2,"future":3}|})));
  let conversion =
    registry 2 [ step 1 [ Rename { parent = []; src = "future"; dst = "known" } ] ]
  in
  print_error (Conversion.upgrade conversion original);
  [%expect
    {|
    (Extension_conflict (payload future))
    (Extension_conflict (payload future))
    (Extension_conflict (payload))
    (Extension_conflict (known))
    |}]
;;

let%expect_test
    "identity arrays preserve order and extensions by host identity across edits"
  =
  let item = shape [ "id"; "text" ] in
  let root =
    Shape.object_ [ "items", Shape.array item ~identity_field:(Some "id") |> ok ] |> ok
  in
  let codec = identity_codec root in
  let original =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"items":[{"id":"a","text":"A","ext":"for-a"},{"id":"b","text":"B","ext":"for-b"}]}}|}
  in
  let carrier = Domain_codec.decode codec original |> ok in
  print_document
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value
          carrier
          (json
             {|{"items":[{"id":"b","text":"edited"},{"id":"a","text":"A"},{"id":"c","text":"new"}]}|})));
  print_error
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value carrier (json {|{"items":[{"id":"b","text":"B"}]}|})));
  print_error
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value
          carrier
          (json {|{"items":[{"id":"a","text":"A"},{"id":"a","text":"duplicate"}]}|})));
  [%expect
    {|
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"items":[{"id":"b","text":"edited","ext":"for-b"},{"id":"a","text":"A","ext":"for-a"},{"id":"c","text":"new"}]}}
    (Extension_conflict (payload items a))
    (Invalid_field (path (payload items)) (reason "duplicate array identity"))
    |}]
;;

let%expect_test
    "unkeyed arrays reject ambiguous changes only when unknown data is present"
  =
  let codec =
    identity_codec
      (Shape.object_
         [ "items", Shape.array (shape [ "text" ]) ~identity_field:None |> ok ]
       |> ok)
  in
  let original =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"items":[{"text":"A","ext":null}]}}|}
  in
  let carrier = Domain_codec.decode codec original |> ok in
  print_document (Domain_codec.encode codec carrier);
  print_error
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value carrier (json {|{"items":[{"text":"changed"}]}|})));
  print_error
    (Domain_codec.encode
       codec
       (Extension_carrier.of_authored_value
          (json {|{"items":[{"text":"new"},{"text":"new2"}]}|})));
  [%expect
    {|
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"items":[{"text":"A","ext":null}]}}
    (Extension_conflict (payload items))
    ok
    |}]
;;

let%expect_test "wrong kind, newer and missing versions never reach domain construction" =
  let codec = identity_codec (shape []) in
  let doc kind version =
    Document.create ~limits ~kind ~version ~payload:(`Object []) |> ok
  in
  print_error (Domain_codec.decode codec (doc "other" 1));
  print_error (Domain_codec.decode codec (doc "example" 2));
  print_error
    (Domain_codec.decode
       codec
       (document
          {|{"format":"ochat.document","schema_version":1,"kind":"example","required_semantics":["must-understand"],"payload":{}}|}));
  let conversion = registry 2 [ step 1 [] ] in
  print_error (Conversion.upgrade conversion (doc "other" 1));
  print_error (Conversion.upgrade conversion (doc "example" 3));
  print_error (Conversion.upgrade (registry 2 []) (doc "example" 1));
  print_error
    (Conversion.create
       ~limits
       ~targets:[ "example", 3 ]
       ~max_steps:3
       ~max_operations:3
       ~steps:[ step 1 [] ]);
  [%expect
    {|
    (Wrong_kind
      (expected example)
      (actual   other))
    (Wrong_version
      (expected 1)
      (actual   2))
    (Required_semantics_unknown must-understand)
    (Unsupported_kind other)
    (Unsupported_version
      (kind    example)
      (version 3)
      (target  2))
    (Missing_conversion
      (kind    example)
      (version 1))
    (Invalid_configuration "conversion chain has a gap")
    |}]
;;

let%expect_test "per-kind targets and registration invariants" =
  let conversion =
    Conversion.create
      ~limits
      ~targets:[ "example", 2; "other", 1 ]
      ~max_steps:2
      ~max_operations:2
      ~steps:[ step 1 [] ]
    |> ok
  in
  print_document
    (Conversion.upgrade
       conversion
       (Document.create ~limits ~kind:"other" ~version:1 ~payload:(`Object []) |> ok));
  List.iter
    [ [ "example", 1; "example", 2 ]; [ "example", 0 ]; [ "", 1 ] ]
    ~f:(fun targets ->
      print_error
        (Conversion.create ~limits ~targets ~max_steps:2 ~max_operations:2 ~steps:[]));
  print_error
    (Conversion.create
       ~limits
       ~targets:[ "example", 2 ]
       ~max_steps:2
       ~max_operations:2
       ~steps:[ step 1 []; step 1 [] ]);
  print_error (Shape.object_ [ "a", Shape.value; "a", Shape.value ]);
  print_error (Shape.array Shape.value ~identity_field:(Some "id"));
  [%expect
    {|
    {"format":"ochat.document","schema_version":1,"kind":"other","payload":{}}
    (Invalid_configuration "targets require unique kinds and positive versions")
    (Invalid_configuration "targets require unique kinds and positive versions")
    (Invalid_configuration "targets require unique kinds and positive versions")
    (Invalid_configuration
     "duplicate/out-of-target step or operation bound exceeded")
    (Invalid_configuration "duplicate owned field: a")
    (Invalid_configuration "identity array requires object elements")
    |}]
;;

let%expect_test "malformed envelope and duplicate keys rejected before lookup" =
  List.iter
    [ {|{"format":"ochat.document","kind":"example","schema_version":1,"schema_version":2,"payload":{}}|}
    ; {|{"format":"ochat.document","kind":"example","schema_version":1,"payload":{"nested":{"x":1,"\u0078":2}}}|}
    ; {|{"format":"other","schema_version":1,"kind":"example","payload":{}}|}
    ; {|{"format":"ochat.document","schema_version":0,"kind":"example","payload":{}}|}
    ; {|{"format":"ochat.document","schema_version":1.0,"kind":"example","payload":{}}|}
    ; {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":null}|}
    ; {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{},"extensions":null}|}
    ; {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{},"required_semantics":["a","a"]}|}
    ; {|{"format":"ochat.document","schema_version":1,"payload":{}}|}
    ; {|[1,2]|}
    ; "old-binary-beta-bytes\000"
    ; "((schema_version 4)(history ()))"
    ]
    ~f:(fun bytes -> print_error (Document.decode ~limits bytes));
  (match Json.decode ~limits "{broken" with
   | Error (Malformed _) -> print_endline "malformed JSON"
   | result -> print_error result);
  [%expect
    {|
    (Duplicate_key (schema_version))
    (Duplicate_key (payload nested x))
    (Unsupported_format other)
    (Invalid_field
      (path (schema_version))
      (reason "positive canonical integer required"))
    (Invalid_field
      (path (schema_version))
      (reason "positive canonical integer required"))
    (Invalid_field (path (payload)) (reason "required object"))
    (Invalid_field (path (extensions)) (reason "optional non-null object"))
    (Invalid_field
      (path (required_semantics))
      (reason "unique nonempty strings required"))
    (Invalid_field (path (kind)) (reason "required nonempty string"))
    (Invalid_field (path ()) (reason "expected envelope object"))
    Unsupported_beta_format
    Unsupported_beta_format
    malformed JSON
    |}]
;;

let%expect_test "input, recursive value, conversion and registry bounds" =
  let small ?(bytes = 1000) ?(depth = 10) ?(fields = 100) ?(nodes = 100) () =
    Limits.create ~max_bytes:bytes ~max_depth:depth ~max_fields:fields ~max_nodes:nodes
    |> ok
  in
  print_error (Limits.create ~max_bytes:1 ~max_depth:257 ~max_fields:1 ~max_nodes:1);
  print_error (Json.decode ~limits:(small ~bytes:2 ()) "[1]");
  print_error (Json.decode ~limits:(small ~depth:2 ()) "[[[0]]]");
  print_error (Json.decode ~limits:(small ~fields:1 ()) {|{"a":1,"b":2}|});
  print_error (Json.decode ~limits:(small ~nodes:2 ()) "[1,2]");
  print_error (Json.decode ~limits:(small ~depth:2 ()) {|"[[[not brackets]]]"|});
  print_error (Json.validate ~limits (`Object [ "x", `Number "nan" ]));
  let original =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{}}|}
  in
  print_error
    (Conversion.upgrade (registry ~max_steps:1 3 [ step 1 []; step 2 [] ]) original);
  print_error
    (Conversion.create
       ~limits
       ~targets:[ "example", 2 ]
       ~max_steps:1
       ~max_operations:1
       ~steps:[ step 1 [ Remove [ "x" ]; Remove [ "y" ] ] ]);
  let bounded = small ~bytes:150 () in
  let conversion =
    registry
      ~limits:bounded
      2
      [ step 1 [ Default { path = [ "large" ]; value = `String (String.make 200 'x') } ] ]
  in
  print_error (Conversion.upgrade conversion original);
  [%expect
    {|
    (Invalid_configuration "positive limits required; depth must be <= 256")
    (Limit_exceeded bytes)
    (Limit_exceeded depth)
    (Limit_exceeded fields)
    (Limit_exceeded nodes)
    ok
    (Invalid_field (path (x)) (reason "invalid JSON number"))
    (Limit_exceeded "conversion steps")
    (Invalid_configuration
     "duplicate/out-of-target step or operation bound exceeded")
    (Limit_exceeded bytes)
    |}]
;;

let%expect_test
    "domain decoder validates after conversion and encode validates replacement"
  =
  let decode payload =
    match Json.field payload ~name:"count" with
    | Value (`Number number) ->
      (match Int.of_string_opt number with
       | Some n when n >= 0 -> Ok n
       | Some _ | None ->
         Error
           (Error.Invalid_field
              { path = [ "count" ]; reason = "nonnegative integer required" }))
    | Absent | Null | Value _ ->
      Error (Error.Invalid_field { path = [ "count" ]; reason = "required count" })
  in
  let codec =
    Domain_codec.create
      ~limits
      ~kind:"example"
      ~version:1
      ~shape:(shape [ "count" ])
      ~supported_semantics:[]
      ~decode
      ~encode:(fun count -> Ok (`Object [ "count", `Number (Int.to_string count) ]))
    |> ok
  in
  let doc payload =
    Document.create ~limits ~kind:"example" ~version:1 ~payload:(json payload) |> ok
  in
  print_error (Domain_codec.decode codec (doc "{}"));
  print_error (Domain_codec.decode codec (doc {|{"count":null}|}));
  print_error (Domain_codec.encode codec (Extension_carrier.of_authored_value (-1)));
  print_document (Domain_codec.encode codec (Extension_carrier.of_authored_value 7));
  [%expect
    {|
    (Invalid_field (path (count)) (reason "required count"))
    (Invalid_field (path (count)) (reason "required count"))
    (Invalid_field (path (count)) (reason "nonnegative integer required"))
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"count":7}}
    |}]
;;

let frame_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Frame.error)]
;;

let record_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Document_record.Error.t)]
;;

let print_record_error = function
  | Ok _ -> print_endline "ok"
  | Error error -> print_s [%sexp (error : Agent_store.Document_record.Error.t)]
;;

let%expect_test
    "stored original bytes anchor conversion independently of Frame v1 checksum"
  =
  let bytes =
    "{ \
     \"kind\":\"example\",\"format\":\"ochat.document\",\"schema_version\":1,\"payload\":{\"old\":null} \
     }"
  in
  let framed =
    Agent_store.Frame.encode ~max_payload_length:(Limits.max_bytes limits) ~flags:3 bytes
    |> frame_ok
  in
  let expected_digest =
    "b9b0e9c373b75b6eff3283bc634552a7701909ad194636291103f5d9ae6b7934"
  in
  require
    [%here]
    (String.equal expected_digest (Agent_store.Document_record.digest bytes));
  let record =
    Agent_store.Document_record.decode_file
      ~limits
      ~expected_digest:(Some expected_digest)
      framed
    |> record_ok
  in
  let conversion =
    registry 2 [ step 1 [ Rename { parent = []; src = "old"; dst = "new" } ] ]
  in
  let upgraded = Agent_store.Document_record.upgrade record ~conversion |> ok in
  print_document (Ok upgraded);
  print_s
    [%sexp (String.equal bytes (Agent_store.Document_record.stored_bytes record) : bool)];
  print_s
    [%sexp
      (String.equal expected_digest (Agent_store.Document_record.stored_digest record)
       : bool)];
  print_s
    [%sexp
      (String.equal
         expected_digest
         (Agent_store.Document_record.digest (Document.to_string upgraded))
       : bool)];
  print_s [%sexp (Agent_store.Frame.current_version : int)];
  print_record_error
    (Agent_store.Document_record.decode_file
       ~limits
       ~expected_digest:(Some (String.make 64 '0'))
       framed);
  let corrupt = Bytes.of_string framed in
  Bytes.set corrupt 21 '!';
  print_record_error
    (Agent_store.Document_record.decode_file
       ~limits
       ~expected_digest:None
       (Bytes.to_string corrupt));
  print_record_error
    (Agent_store.Document_record.decode_file
       ~limits
       ~expected_digest:None
       (framed ^ "trailing"));
  print_record_error
    (Agent_store.Document_record.decode_file
       ~limits
       ~expected_digest:None
       (String.drop_suffix framed 1));
  print_record_error
    (Agent_store.Document_record.decode_file
       ~limits
       ~expected_digest:(Some "invalid")
       framed);
  [%expect
    {|
    {"kind":"example","format":"ochat.document","schema_version":2,"payload":{"new":null}}
    true
    true
    false
    1
    (Digest_mismatch
      (expected 0000000000000000000000000000000000000000000000000000000000000000)
      (actual b9b0e9c373b75b6eff3283bc634552a7701909ad194636291103f5d9ae6b7934))
    (Frame Checksum_mismatch)
    Trailing_bytes
    Incomplete_frame
    (Invalid_digest invalid)
    |}]
;;

let%expect_test
    "frame offsets, independent newer Frame version, and unsupported beta bytes"
  =
  let doc =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{}}|}
  in
  let framed = Agent_store.Document_record.encode doc ~limits ~flags:0 |> record_ok in
  let record, next =
    Agent_store.Document_record.decode_frame
      ~limits
      ~contents:("prefix" ^ framed ^ "tail")
      ~offset:6
      ~expected_digest:None
    |> record_ok
  in
  print_s [%sexp (Int.equal next (6 + String.length framed) : bool)];
  print_endline (Document.to_string (Agent_store.Document_record.document record));
  let newer = Bytes.of_string framed in
  Bytes.set newer 9 '\002';
  print_record_error
    (Agent_store.Document_record.decode_file
       ~limits
       ~expected_digest:None
       (Bytes.to_string newer));
  let beta =
    Agent_store.Frame.encode ~max_payload_length:100 ~flags:0 "\001\002beta" |> frame_ok
  in
  print_record_error
    (Agent_store.Document_record.decode_file ~limits ~expected_digest:None beta);
  [%expect
    {|
    true
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{}}
    (Frame (Unsupported_version 2))
    (Document Unsupported_beta_format)
    |}]
;;

let%expect_test
    "nullable structured values preserve nested extensions and distinguish absence"
  =
  let codec =
    identity_codec (Shape.object_ [ "optional", Shape.nullable (shape [ "text" ]) ] |> ok)
  in
  List.iter
    [ "{}"; {|{"optional":null}|}; {|{"optional":{"text":"a","unknown":null}}|} ]
    ~f:(fun payload ->
      let doc =
        Document.create ~limits ~kind:"example" ~version:1 ~payload:(json payload) |> ok
      in
      let carrier = Domain_codec.decode codec doc |> ok in
      print_document (Domain_codec.encode codec carrier));
  let doc =
    Document.create
      ~limits
      ~kind:"example"
      ~version:1
      ~payload:(json {|{"optional":{"text":"a","unknown":null}}|})
    |> ok
  in
  let carrier = Domain_codec.decode codec doc |> ok in
  print_error
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value carrier (json {|{"optional":null}|})));
  [%expect
    {|
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{}}
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"optional":null}}
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"optional":{"text":"a","unknown":null}}}
    (Extension_conflict (payload optional))
    |}]
;;

let%expect_test "unkeyed array object field normalization is semantically unchanged" =
  let codec =
    identity_codec
      (Shape.object_
         [ "items", Shape.array (shape [ "a"; "b" ]) ~identity_field:None |> ok ]
       |> ok)
  in
  let doc =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"items":[{"a":1,"b":null,"unknown":2}]}}|}
  in
  let carrier = Domain_codec.decode codec doc |> ok in
  print_document
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value carrier (json {|{"items":[{"b":null,"a":1}]}|})));
  print_s [%sexp (Json.equal (json {|{"a":null}|}) (json "{}") : bool)];
  print_s [%sexp (Json.equal (json "[1,2]") (json "[2,1]") : bool)];
  print_s [%sexp (Json.equal (json "1.0") (json "1") : bool)];
  [%expect
    {|
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"items":[{"a":1,"b":null,"unknown":2}]}}
    false
    false
    false
    |}]
;;

let%expect_test "overlapping moves reject information loss" =
  let doc =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"a":{"b":1}}}|}
  in
  List.iter
    [ [ "a" ], [ "a"; "c" ]; [ "a"; "b" ], [ "a" ]; [ "a" ], [ "a" ]; [], [ "x" ] ]
    ~f:(fun (src, dst) ->
      print_error (Conversion.upgrade (registry 2 [ step 1 [ Move { src; dst } ] ]) doc));
  [%expect
    {|
    (Invalid_field (path (a)) (reason "move paths must be nonempty and disjoint"))
    (Invalid_field
      (path (a b))
      (reason "move paths must be nonempty and disjoint"))
    (Invalid_field (path (a)) (reason "move paths must be nonempty and disjoint"))
    (Invalid_field (path ()) (reason "move paths must be nonempty and disjoint"))
    |}]
;;

let%expect_test
    "callback output is bounded, malformed outputs rejected, typed failure and \
     unexpected exceptions retained"
  =
  let doc =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{}}|}
  in
  let convert f =
    Conversion.Step.of_function ~kind:"example" ~from_version:1 ~f
    |> ok
    |> List.return
    |> registry 2
  in
  print_error
    (Conversion.upgrade (convert (fun _ -> Ok (`Object [ "x", `Null; "x", `Null ]))) doc);
  print_error (Conversion.upgrade (convert (fun _ -> Ok (`Array []))) doc);
  print_error
    (Conversion.upgrade
       (convert (fun _ ->
          Error
            (Error.Invalid_field { path = [ "needed" ]; reason = "information absent" })))
       doc);
  (try
     ignore
       (Conversion.upgrade (convert (fun _ -> failwith "converter-bug")) doc
        : (Document.t, Error.t) Result.t)
   with
   | Failure message -> print_endline message);
  [%expect
    {|
    (Duplicate_key (x))
    (Invalid_field (path (payload)) (reason "required object"))
    (Invalid_field (path (needed)) (reason "information absent"))
    converter-bug
    |}]
;;

let%expect_test "known required semantics are accepted but invalid shapes remain rejected"
  =
  let codec = identity_codec ~semantics:[ "safe" ] (shape []) in
  print_error
    (Domain_codec.decode
       codec
       (document
          {|{"format":"ochat.document","schema_version":1,"kind":"example","required_semantics":["safe"],"payload":{}}|}));
  print_error
    (Domain_codec.decode
       (identity_codec (Shape.object_ [ "object", shape [] ] |> ok))
       (document
          {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"object":false}}|}));
  [%expect
    {|
    ok
    (Invalid_field
      (path (payload object))
      (reason "value does not match codec shape"))
    |}]
;;

let%expect_test
    "property: arbitrary extensions survive edits through actual scalar domain values"
  =
  let codec =
    Domain_codec.create
      ~limits
      ~kind:"example"
      ~version:1
      ~shape:(shape [ "count" ])
      ~supported_semantics:[]
      ~decode:(fun payload ->
        match Json.field payload ~name:"count" with
        | Value (`Number value) ->
          (match Int.of_string_opt value with
           | Some value -> Ok value
           | None -> Error (Error.Malformed "bad count"))
        | Absent | Null | Value _ -> Error (Error.Malformed "missing count"))
      ~encode:(fun count -> Ok (`Object [ "count", `Number (Int.to_string count) ]))
    |> ok
  in
  Quickcheck.test
    ~trials:100
    (Quickcheck.Generator.tuple3
       Int.quickcheck_generator
       Int.quickcheck_generator
       (Quickcheck.Generator.map
          String.Utf8.quickcheck_generator
          ~f:String.Utf8.to_string))
    ~f:(fun (before, after, opaque) ->
      let payload =
        `Object
          [ "future", `Object [ "bytes", `String opaque; "explicit_null", `Null ]
          ; "count", `Number (Int.to_string before)
          ]
      in
      let doc = Document.create ~limits ~kind:"example" ~version:1 ~payload |> ok in
      let carrier = Domain_codec.decode codec doc |> ok in
      let changed =
        Domain_codec.encode codec (Extension_carrier.with_value carrier after) |> ok
      in
      require
        [%here]
        (Int.equal
           after
           (Domain_codec.decode codec changed |> ok |> Extension_carrier.value));
      match
        ( Json.field (Document.payload changed) ~name:"future"
        , Json.field payload ~name:"future" )
      with
      | Value actual, Value expected -> require [%here] (Json.equal actual expected)
      | _ -> require [%here] false);
  print_endline "100 extension-preserving domain edits";
  [%expect {| 100 extension-preserving domain edits |}]
;;

let%expect_test "in-memory values validate UTF-8 and byte bounds before serialization" =
  print_error (Json.validate ~limits (`String "\255"));
  print_error (Json.validate ~limits (`Object [ "\255", `Null ]));
  let small =
    Limits.create ~max_bytes:100 ~max_depth:3 ~max_fields:3 ~max_nodes:4 |> ok
  in
  print_error (Json.validate ~limits:small (`String (String.make 101 'x')));
  print_error (Document.decode ~limits:small (String.make 101 'x'));
  [%expect
    {|
    (Invalid_field (path ()) (reason "invalid UTF-8 string"))
    (Invalid_field (path ("\255")) (reason "invalid UTF-8 object key"))
    (Limit_exceeded bytes)
    (Limit_exceeded bytes)
    |}]
;;

let%expect_test "numeric wire lexemes have exact syntax and exact byte-bound admission" =
  let single_byte =
    Limits.create ~max_bytes:1 ~max_depth:1 ~max_fields:1 ~max_nodes:1 |> ok
  in
  print_error (Json.decode ~limits:single_byte "1");
  print_error (Json.validate ~limits:single_byte (`Number "1"));
  List.iter
    [ " 1 "; "1 2"; "01"; "NaN"; "true"; "+1"; ".1"; "1."; "1e"; "1e+" ]
    ~f:(fun number -> print_error (Json.validate ~limits (`Number number)));
  print_error (Json.validate ~limits (`Number (String.make 300 '[')));
  [%expect
    {|
    ok
    ok
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    (Invalid_field (path ()) (reason "invalid JSON number"))
    |}]
;;

let%expect_test "encoded byte bounds include exact escaping and structural punctuation" =
  let limits_for max_bytes =
    Limits.create ~max_bytes ~max_depth:10 ~max_fields:100 ~max_nodes:100 |> ok
  in
  List.iter
    [ "empty string", `String "", 2
    ; "empty object", `Object [], 2
    ; "empty array", `Array [], 2
    ; "null", `Null, 4
    ; "true", `True, 4
    ; "false", `False, 5
    ; "number", `Number "-1.25e+2", 8
    ; "short escapes and slash", `String "\"\\\b\012\n\r\t/", 17
    ; "unicode control escapes", `String "\000\001\031", 20
    ; "UTF-8", `String "é😀", 8
    ; "escaped key", `Object [ "\000\"", `Null ], 17
    ; "object separators", `Object [ "a", `Null; "b", `False ], 20
    ; "array separators", `Array [ `True; `False; `Null ], 17
    ]
    ~f:(fun (name, json, bytes) ->
      let encoded = Jsonaf.to_string json in
      require [%here] (Int.equal bytes (String.length encoded));
      require [%here] (Result.is_ok (Json.validate ~limits:(limits_for bytes) json));
      require [%here] (Result.is_ok (Json.decode ~limits:(limits_for bytes) encoded));
      require
        [%here]
        (Result.equal
           Unit.equal
           Error.equal
           (Json.validate ~limits:(limits_for (bytes - 1)) json)
           (Error (Error.Limit_exceeded "bytes")));
      print_endline name);
  [%expect
    {|
    empty string
    empty object
    empty array
    null
    true
    false
    number
    short escapes and slash
    unicode control escapes
    UTF-8
    escaped key
    object separators
    array separators
    |}]
;;

let%expect_test "over-limit escaped strings and keys reject without encoded allocation" =
  let max_bytes = 1024 * 1024 in
  let limits = Limits.create ~max_bytes ~max_depth:2 ~max_fields:2 ~max_nodes:2 |> ok in
  let text = String.make (max_bytes - 1) '\000' in
  List.iter
    [ `String text; `Object [ text, `Null ] ]
    ~f:(fun json ->
      let before = Gc.allocated_bytes () in
      let result = Json.validate ~limits json in
      let allocated_bytes = Gc.allocated_bytes () -. before in
      (* A megabyte margin tolerates small validation bookkeeping while catching
       materialization of the six-megabyte escaped representation. *)
      require [%here] Float.(allocated_bytes < of_int max_bytes);
      print_error result);
  [%expect
    {|
    (Limit_exceeded bytes)
    (Limit_exceeded bytes)
    |}]
;;

let%expect_test "array identity ownership cannot change underneath retained extensions" =
  let codec identity_field =
    identity_codec
      (Shape.object_
         [ ( "items"
           , Shape.array (shape [ "id"; "other_id"; "value" ]) ~identity_field |> ok )
         ]
       |> ok)
  in
  let doc =
    document
      {|{"format":"ochat.document","schema_version":1,"kind":"example","payload":{"items":[{"id":"a","other_id":"b","value":1,"future":17}]}}|}
  in
  List.iter
    [ None, Some "id"; Some "id", None; Some "id", Some "other_id" ]
    ~f:(fun (previous_identity, next_identity) ->
      let carrier = Domain_codec.decode (codec previous_identity) doc |> ok in
      print_error (Domain_codec.encode (codec next_identity) carrier));
  let no_unknown =
    Document.create
      ~limits
      ~kind:"example"
      ~version:1
      ~payload:(json {|{"items":[{"id":"a","other_id":"b","value":1}]}|})
    |> ok
  in
  let carrier = Domain_codec.decode (codec None) no_unknown |> ok in
  print_document (Domain_codec.encode (codec (Some "id")) carrier);
  [%expect
    {|
    (Extension_conflict (payload items))
    (Extension_conflict (payload items))
    (Extension_conflict (payload items))
    {"format":"ochat.document","schema_version":1,"kind":"example","payload":{"items":[{"id":"a","other_id":"b","value":1}]}}
    |}]
;;
