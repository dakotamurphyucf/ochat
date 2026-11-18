open! Core
open Document_schema

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Error.t)]
;;

let limits = Limits.default
let row_shape = Shape.object_ [ "id", Shape.value; "text", Shape.value ] |> ok

let shape =
  Shape.object_
    [ "text", Shape.value
    ; "number", Shape.value
    ; "rows", Shape.array row_shape ~identity_field:(Some "id") |> ok
    ]
  |> ok
;;

let codec ?(limits = limits) ?(version = 9) () =
  Domain_codec.create_validated
    ~limits
    ~kind:"measured.example"
    ~version
    ~shape
    ~supported_semantics:[ "kept" ]
    ~validate:(fun _ -> Ok ())
    ~decode:(fun json -> Ok json)
    ~encode:(fun json -> Ok json)
  |> ok
;;

let row text = `Object [ "id", `String "r1"; "text", `String text ]

let payload text =
  `Object [ "text", text; "number", `Number "1e+02"; "rows", `Array [ row "before" ] ]
;;

let envelope payload =
  `Object
    [ "future\n\"", `Object [ "nested", `Array [ `String "\000💡\\"; `Number "7.00" ] ]
    ; "kind", `String "measured.example"
    ; "format", `String Document.format
    ; "schema_version", `Number "9"
    ; "required_semantics", `Array [ `String "kept" ]
    ; "extensions", `Object [ "opaque", `Null ]
    ; "payload", payload
    ]
;;

(* Independent complete-envelope construction and ordinary full inspection. *)
let reference document ~limits payload =
  match Document.json document with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, value) ->
         name, if String.equal name "payload" then payload else value))
    |> Document.inspect ~limits
  | _ -> assert false
;;

let encoded codec document payload =
  let%bind.Result carrier = Domain_codec.decode codec document in
  Domain_codec.encode codec (Extension_carrier.with_value carrier payload)
;;

let result_bytes = Result.map ~f:Document.to_string

let check document ~limits ~known ~expected =
  (* This public no-growth route retains exact JSON/admission but intentionally
     has no fresh scan measurements. It independently exercises full fallback. *)
  let unmeasured =
    Document.replace_payload_scalars document ~limits:Limits.default ~updates:[] |> ok
  in
  assert (String.equal (Document.to_string document) (Document.to_string unmeasured));
  let expected = reference document ~limits expected |> result_bytes in
  let codec = codec ~limits () in
  let exact_profile = Document.inspect ~limits (Document.json document) |> Result.ok in
  List.iter (document :: unmeasured :: Option.to_list exact_profile) ~f:(fun document ->
    let actual = encoded codec document known |> result_bytes in
    assert (Result.equal String.equal Error.equal expected actual))
;;

let rec dimensions = function
  | `Object fields ->
    List.fold
      fields
      ~init:(List.length fields, 1, 1)
      ~f:(fun (fields, nodes, depth) (_, child) ->
        let child_fields, child_nodes, child_depth = dimensions child in
        fields + child_fields, nodes + child_nodes, Int.max depth (child_depth + 1))
  | `Array values ->
    List.fold values ~init:(0, 1, 1) ~f:(fun (fields, nodes, depth) child ->
      let child_fields, child_nodes, child_depth = dimensions child in
      fields + child_fields, nodes + child_nodes, Int.max depth (child_depth + 1))
  | `Null | `True | `False | `Number _ | `String _ -> 0, 1, 1
;;

let%expect_test "measured codec replacement equals complete inspection and fallback" =
  let original = Document.inspect ~limits (envelope (payload (`String "before"))) |> ok in
  let original_bytes = Document.to_string original in
  let edited = payload (`String "after\000\n\"\\💡") in
  check original ~limits ~known:edited ~expected:edited;
  (* General object merge restores original member order even without unknown
     payload fields. Reusing the differently ordered known JSON would be wrong. *)
  let reordered =
    match edited with
    | `Object fields -> `Object (List.rev fields)
    | _ -> assert false
  in
  check original ~limits ~known:reordered ~expected:edited;
  let nested_reordered =
    `Object
      [ "rows", `Array [ `Object [ "text", `String "before"; "id", `String "r1" ] ]
      ; "number", `Number "1e+02"
      ; "text", `String "after\000\n\"\\💡"
      ]
  in
  check original ~limits ~known:nested_reordered ~expected:edited;
  (* Added/removed owned fields are already represented in the new known
     measurements; the merge still retains the actual old member order. *)
  let sparse =
    Document.inspect
      ~limits
      (envelope (`Object [ "rows", `Array [ row "before" ]; "text", `String "before" ]))
    |> ok
  in
  check
    sparse
    ~limits
    ~known:(`Object [ "number", `Number "1e+02"; "text", `Null; "rows", `Array [] ])
    ~expected:(`Object [ "rows", `Array []; "text", `Null; "number", `Number "1e+02" ]);
  check
    original
    ~limits
    ~known:(`Object [ "rows", `Array []; "text", `Null ])
    ~expected:(`Object [ "text", `Null; "rows", `Array [] ]);
  (* Decode obtains measurements in its existing JSON walk, not only inspect. *)
  let decoded = Document.decode ~limits original_bytes |> ok in
  check decoded ~limits ~known:edited ~expected:edited;
  let deep =
    List.fold (List.init 8 ~f:Fn.id) ~init:(`String "escaped\n") ~f:(fun value _ ->
      `Array [ value ])
  in
  let grown =
    `Object
      [ "text", deep
      ; "number", `Number "1e+02"
      ; ( "rows"
        , `Array
            [ row "before"; `Object [ "id", `String "r2"; "text", `String "second" ] ] )
      ]
  in
  let full = reference original ~limits grown |> ok in
  let fields, nodes, depth = dimensions (Document.json full) in
  let bytes = String.length (Document.to_string full) in
  let profile bytes depth fields nodes =
    Limits.create ~max_bytes:bytes ~max_depth:depth ~max_fields:fields ~max_nodes:nodes
    |> ok
  in
  List.iter
    [ profile bytes depth fields nodes
    ; profile (bytes - 1) depth fields nodes
    ; profile bytes (depth - 1) fields nodes
    ; profile bytes depth (fields - 1) nodes
    ; profile bytes depth fields (nodes - 1)
    ]
    ~f:(fun limits -> check original ~limits ~known:grown ~expected:grown);
  (* Compare positive exact measurements and the unmeasured fallback against
     the independent full validator, including its first error when several
     bounds fail. No metric can choose a replacement error for that scan. *)
  let original_fields, original_nodes, original_depth =
    dimensions (Document.json original)
  in
  let original_size = String.length original_bytes in
  let unmeasured =
    Document.replace_payload_scalars original ~limits ~updates:[ [ "text" ], `Null ] |> ok
  in
  List.iter
    [ profile original_size original_depth original_fields original_nodes
    ; profile (original_size - 1) original_depth original_fields original_nodes
    ; profile original_size (original_depth - 1) original_fields original_nodes
    ; profile original_size original_depth (original_fields - 1) original_nodes
    ; profile original_size original_depth original_fields (original_nodes - 1)
    ; profile 1 1 1 1
    ]
    ~f:(fun limits ->
      List.iter [ original; unmeasured ] ~f:(fun document ->
        assert (
          Result.equal
            Unit.equal
            Error.equal
            (Document.validate document ~limits)
            (Json.validate ~limits (Document.json document)))));
  (* An invalid original is rejected before encoding even when the replacement
     would fit. Reuse cannot turn the template check into a candidate-only check. *)
  let large =
    Document.inspect ~limits (envelope (payload (`String (String.make 512 'x')))) |> ok
  in
  let tighter = profile (String.length (Document.to_string large) - 1) 64 1000 10000 in
  reference large ~limits:tighter (payload `Null) |> ok |> ignore;
  let large_carrier = Domain_codec.decode (codec ()) large |> ok in
  let smaller = Extension_carrier.with_value large_carrier (payload `Null) in
  assert (
    Result.equal
      Unit.equal
      Error.equal
      (Domain_codec.encode (codec ~limits:tighter ()) smaller |> Result.map ~f:ignore)
      (Json.validate ~limits:tighter (Document.json large)));
  (* A stricter, still sufficient profile may reuse exact whole measurements. *)
  check original ~limits:(profile 4096 64 1000 10000) ~known:edited ~expected:edited;
  let with_unknown =
    let raw =
      `Object
        [ "text", `String "before"
        ; "number", `Number "1e+02"
        ; ( "rows"
          , `Array
              [ `Object
                  [ "id", `String "r1"
                  ; "text", `String "before"
                  ; "future", `Number "2e0"
                  ]
              ] )
        ]
    in
    Document.inspect ~limits (envelope raw) |> ok
  in
  let known =
    `Object
      [ "text", `String "after"
      ; "number", `Number "1e+02"
      ; "rows", `Array [ row "after" ]
      ]
  in
  let preserved =
    `Object
      [ "text", `String "after"
      ; "number", `Number "1e+02"
      ; ( "rows"
        , `Array
            [ `Object
                [ "id", `String "r1"; "text", `String "after"; "future", `Number "2e0" ]
            ] )
      ]
  in
  check with_unknown ~limits ~known ~expected:preserved;
  (* A retained child must not leave an underestimated exact measurement that
     a later stricter-profile validation could incorrectly accept. *)
  let preserved_document = encoded (codec ()) with_unknown known |> ok in
  let below_preserved =
    profile (String.length (Document.to_string preserved_document) - 1) 64 1000 10000
  in
  assert (
    Result.equal
      Unit.equal
      Error.equal
      (Document.validate preserved_document ~limits:below_preserved)
      (Json.validate ~limits:below_preserved (Document.json preserved_document)));
  let removed =
    `Object [ "text", `String "after"; "number", `Number "1e+02"; "rows", `Array [] ]
  in
  assert (
    Result.equal
      Unit.equal
      Error.equal
      (encoded (codec ()) with_unknown removed |> Result.map ~f:ignore)
      (Error (Error.Extension_conflict [ "payload"; "rows"; "r1" ])));
  assert (
    Result.equal
      Unit.equal
      Error.equal
      (encoded (codec ~version:10 ()) original edited |> Result.map ~f:ignore)
      (Error (Error.Wrong_version { expected = 10; actual = 9 })));
  List.iter
    [ `Object [ "text", `String "\255"; "number", `Number "1e+02"; "rows", `Array [] ]
    ; `Object [ "text", `Null; "number", `Number "01"; "rows", `Array [] ]
    ; `Object
        [ "text", `Null; "text", `True; "number", `Number "1e+02"; "rows", `Array [] ]
    ]
    ~f:(fun malformed ->
      assert (
        Result.equal
          Unit.equal
          Error.equal
          (encoded (codec ()) original malformed |> Result.map ~f:ignore)
          (Json.validate ~limits malformed)));
  assert (String.equal original_bytes (Document.to_string original));
  print_endline
    "exact raw output, envelope extensions, order, four bounds and errors conserved";
  [%expect
    {| exact raw output, envelope extensions, order, four bounds and errors conserved |}]
;;

exception Scan_interrupted of unit ref

let%expect_test
    "document measurement keeps the ordinary scan checkpoints and interruption"
  =
  let payload = `Object [ "values", `Array (List.init 1024 ~f:(fun _ -> `Null)) ] in
  let raw =
    `Object
      [ "format", `String Document.format
      ; "schema_version", `Number "1"
      ; "kind", `String "checkpoint.example"
      ; "payload", payload
      ]
  in
  let calls = ref 0 in
  Json.validate_and_measure_with_checkpoint ~limits ~checkpoint:(fun () -> incr calls) raw
  |> ok
  |> ignore;
  let expected = !calls in
  calls := 0;
  Document.create_with_checkpoint
    ~limits
    ~checkpoint:(fun () -> incr calls)
    ~kind:"checkpoint.example"
    ~version:1
    ~payload
  |> ok
  |> ignore;
  assert (!calls = expected);
  let marker = ref () in
  let checkpoint () = raise (Scan_interrupted marker) in
  (match
     Document.create_with_checkpoint
       ~limits
       ~checkpoint
       ~kind:"checkpoint.example"
       ~version:1
       ~payload
   with
   | _ -> failwith "checkpoint interruption was swallowed"
   | exception Scan_interrupted actual -> assert (phys_equal actual marker));
  print_endline "same checkpoint count and exact interruption value";
  [%expect {| same checkpoint count and exact interruption value |}]
;;
