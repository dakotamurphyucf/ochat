open! Core
open Document_schema

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Error.t)]
;;

let limits = Limits.default
let shape fields = Shape.object_ fields |> ok
let json text = Json.decode ~limits text |> ok
let doc text = Document.decode ~limits text |> ok

let print_result = function
  | Ok document -> print_endline (Document.to_string document)
  | Error error -> print_s [%sexp (error : Error.t)]
;;

let case fields = shape (("tag", Shape.value) :: fields)

let tagged () =
  Shape.tagged_object
    ~discriminator:"tag"
    [ "pending", case [ "id", Shape.value ]
    ; "finished", case [ "id", Shape.value; "message", Shape.value ]
    ]
  |> ok
;;

let codec shape =
  Domain_codec.create
    ~limits
    ~kind:"tagged.example"
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~decode:Result.return
    ~encode:Result.return
  |> ok
;;

let%expect_test "tagged shapes validate case ownership and array identities" =
  let rejected cases =
    Shape.tagged_object ~discriminator:"tag" cases |> Result.is_error
  in
  print_s
    [%sexp
      (( rejected []
       , rejected [ "", case [] ]
       , rejected [ "pending", Shape.value ]
       , rejected [ "pending", shape [ "other", Shape.value ] ]
       , rejected [ "pending", case []; "pending", case [] ]
       , Shape.tagged_object ~discriminator:"" [ "pending", case [] ] |> Result.is_error
       , Shape.array (tagged ()) ~identity_field:(Some "id") |> Result.is_ok
       , Shape.array (tagged ()) ~identity_field:(Some "missing") |> Result.is_error )
       : bool * bool * bool * bool * bool * bool * bool * bool)];
  [%expect {| (true true true true true true true true) |}]
;;

let%expect_test "a selected case preserves other-case fields as unknown data" =
  let codec = codec (tagged ()) in
  let restored =
    Domain_codec.decode
      codec
      (doc
         {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"tag":"pending","id":"a","message":"future meaning","extra":null},"future_envelope":true}|})
    |> ok
  in
  print_endline (Jsonaf.to_string (Extension_carrier.value restored));
  print_result
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value restored (json {|{"tag":"pending","id":"renamed"}|})));
  print_result
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value
          restored
          (json {|{"tag":"finished","id":"a","message":"done"}|})));
  [%expect
    {|
    {"tag":"pending","id":"a"}
    {"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"tag":"pending","id":"renamed","message":"future meaning","extra":null},"future_envelope":true}
    (Extension_conflict (payload))
    |}]
;;

let%expect_test "tag changes are allowed without unknown payload fields" =
  let codec = codec (tagged ()) in
  let restored =
    Domain_codec.decode
      codec
      (doc
         {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"tag":"pending","id":"a"},"future_envelope":true}|})
    |> ok
  in
  print_result
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value
          restored
          (json {|{"tag":"finished","id":"a","message":"done"}|})));
  List.iter
    [ {|{"id":"a"}|}; {|{"tag":null,"id":"a"}|}; {|{"tag":"future","id":"a"}|} ]
    ~f:(fun text ->
      let document =
        Document.create ~limits ~kind:"tagged.example" ~version:1 ~payload:(json text)
        |> ok
      in
      match Domain_codec.decode codec document with
      | Ok _ -> print_endline "unexpected success"
      | Error error -> print_s [%sexp (error : Error.t)]);
  [%expect
    {|
    {"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"tag":"finished","id":"a","message":"done"},"future_envelope":true}
    (Invalid_field (path (payload tag))
     (reason "required nonempty array identity string"))
    (Invalid_field (path (payload tag))
     (reason "required nonempty array identity string"))
    (Invalid_field (path (payload tag)) (reason "unsupported discriminator"))
    |}]
;;

let%expect_test "tagged arrays retain keyed extensions through reorder and edits" =
  let array = Shape.array (tagged ()) ~identity_field:(Some "id") |> ok in
  let codec = codec (shape [ "items", array ]) in
  let restored =
    Domain_codec.decode
      codec
      (doc
         {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"items":[{"id":"a","tag":"pending","future":"for-a"},{"id":"b","tag":"finished","message":"before","future":"for-b"}]}}|})
    |> ok
  in
  print_result
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value
          restored
          (json
             {|{"items":[{"id":"b","tag":"finished","message":"after"},{"id":"a","tag":"pending"}]}|})));
  print_result
    (Domain_codec.encode
       codec
       (Extension_carrier.with_value
          restored
          (json
             {|{"items":[{"id":"a","tag":"finished","message":"changed"},{"id":"b","tag":"finished","message":"before"}]}|})));
  [%expect
    {|
    {"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"items":[{"id":"b","tag":"finished","message":"after","future":"for-b"},{"id":"a","tag":"pending","future":"for-a"}]}}
    (Extension_conflict (payload items a))
    |}]
;;

let%expect_test "polymorphic carrier projection preserves template and field ownership" =
  let codec = codec (shape [ "text", Shape.value ]) in
  let restored =
    Domain_codec.decode
      codec
      (doc
         {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"text":"before","future":{"n":null}},"future_envelope":7}|})
    |> ok
  in
  let transport : unit Extension_carrier.t = Extension_carrier.with_value restored () in
  let numbered : int Extension_carrier.t = Extension_carrier.with_value transport 42 in
  print_s [%sexp (Extension_carrier.value numbered : int)];
  let edited = Extension_carrier.with_value numbered (json {|{"text":"after"}|}) in
  print_result (Domain_codec.encode codec edited);
  print_result (Domain_codec.encode codec restored);
  [%expect
    {|
    42
    {"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"text":"after","future":{"n":null}},"future_envelope":7}
    {"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"text":"before","future":{"n":null}},"future_envelope":7}
    |}]
;;

let%expect_test "empty dictionary keys retain stable identity through edits and reorder" =
  let element = shape [ "key", Shape.value; "value", Shape.value ] in
  let permissive =
    Shape.array ~allow_empty_identity:true element ~identity_field:(Some "key") |> ok
  in
  let strict = Shape.array element ~identity_field:(Some "key") |> ok in
  let permissive_codec = codec (shape [ "entries", permissive ]) in
  let strict_codec = codec (shape [ "entries", strict ]) in
  let restored =
    Domain_codec.decode
      permissive_codec
      (doc
         {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"entries":[{"key":"","value":1,"extra":"empty"},{"key":"a","value":2,"extra":"named"}]}}|})
    |> ok
  in
  print_result
    (Domain_codec.encode
       permissive_codec
       (Extension_carrier.with_value
          restored
          (json {|{"entries":[{"key":"a","value":3},{"key":"","value":4}]}|})));
  let nonempty =
    Domain_codec.decode
      permissive_codec
      (doc
         {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"entries":[{"key":"a","value":1,"extra":null}]}}|})
    |> ok
  in
  print_result (Domain_codec.encode strict_codec nonempty);
  let empty =
    Document.create
      ~limits
      ~kind:"tagged.example"
      ~version:1
      ~payload:(json {|{"entries":[{"key":"","value":1}]}|})
    |> ok
  in
  print_s [%sexp (Domain_codec.decode strict_codec empty |> Result.is_error : bool)];
  [%expect
    {|
    {"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"entries":[{"key":"a","value":3,"extra":"named"},{"key":"","value":4,"extra":"empty"}]}}
    (Extension_conflict (payload entries))
    true
    |}]
;;

let%expect_test "adoption carries checkpoint and journal extensions by current identity" =
  let codec =
    codec
      (shape
         [ ( "items"
           , Shape.array
               (shape [ "id", Shape.value; "value", Shape.value ])
               ~identity_field:(Some "id")
             |> ok )
         ])
  in
  let previous =
    Domain_codec.decode
      codec
      (doc
         {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"items":[{"id":"a","value":1,"old":null},{"id":"b","value":2}],"prior":true},"old_envelope":null}|})
    |> ok
  in
  let incoming =
    Domain_codec.decode
      codec
      (doc
         {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"items":[{"id":"b","value":3,"new":"retained"},{"id":"a","value":1}],"next":true},"new_envelope":true}|})
    |> ok
  in
  let adopted = Domain_codec.adopt codec ~previous ~incoming |> ok in
  print_result (Domain_codec.encode codec adopted);
  [%expect
    {| {"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"items":[{"id":"b","value":3,"new":"retained"},{"id":"a","value":1,"old":null}],"prior":true,"next":true},"old_envelope":null,"new_envelope":true} |}]
;;

let%expect_test
    "adoption rejects unknown value conflicts and destructive projection changes"
  =
  let codec =
    codec
      (shape
         [ ( "items"
           , Shape.array (shape [ "id", Shape.value ]) ~identity_field:(Some "id") |> ok )
         ])
  in
  let previous_document =
    doc
      {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"items":[{"id":"a","future":{"a":1}}]}}|}
  in
  let previous = Domain_codec.decode codec previous_document |> ok in
  let incoming =
    Domain_codec.decode
      codec
      (doc
         {|{"format":"ochat.document","kind":"tagged.example","schema_version":1,"payload":{"items":[{"id":"a","future":{"b":2}}]}}|})
    |> ok
  in
  let conflicting = Domain_codec.adopt codec ~previous ~incoming |> Result.is_error in
  let deleted = Extension_carrier.of_authored_value (json {|{"items":[]}|}) in
  let deletion =
    Domain_codec.adopt codec ~previous ~incoming:deleted |> Result.is_error
  in
  let unchanged =
    Domain_codec.encode codec previous
    |> ok
    |> Document.json
    |> Json.equal (Document.json previous_document)
  in
  print_s [%sexp { conflicting : bool; deletion : bool; unchanged : bool }];
  [%expect {| ((conflicting true) (deletion true) (unchanged true)) |}]
;;

let%expect_test "adoption enforces the combined document limit without mutating inputs" =
  let limits =
    Limits.create ~max_bytes:260 ~max_depth:20 ~max_fields:50 ~max_nodes:100 |> ok
  in
  let codec =
    Domain_codec.create
      ~limits
      ~kind:"bounded.adopt"
      ~version:1
      ~shape:(shape [ "id", Shape.value ])
      ~supported_semantics:[]
      ~decode:Result.return
      ~encode:Result.return
    |> ok
  in
  let document name =
    Document.create
      ~limits
      ~kind:"bounded.adopt"
      ~version:1
      ~payload:(`Object [ "id", `String "a"; name, `String (String.make 90 'x') ])
    |> ok
  in
  let left = document "left" in
  let right = document "right" in
  let previous = Domain_codec.decode codec left |> ok in
  let incoming = Domain_codec.decode codec right |> ok in
  let rejected =
    match Domain_codec.adopt codec ~previous ~incoming with
    | Error (Error.Limit_exceeded _) -> true
    | Error _ | Ok _ -> false
  in
  let left_unchanged =
    Domain_codec.encode codec previous
    |> ok
    |> Document.json
    |> Json.equal (Document.json left)
  in
  let right_unchanged =
    Domain_codec.encode codec incoming
    |> ok
    |> Document.json
    |> Json.equal (Document.json right)
  in
  print_s [%sexp { rejected : bool; left_unchanged : bool; right_unchanged : bool }];
  [%expect {| ((rejected true) (left_unchanged true) (right_unchanged true)) |}]
;;
