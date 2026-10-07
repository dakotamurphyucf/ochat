open! Core
module P = History_entry.Payload

let ok = Result.ok_or_failwith
let payload source = Jsonaf.of_string source |> P.of_json |> ok

let%test_unit "local tool results are authored and lower to exact legacy output DTOs" =
  let module R = Openai.Responses in
  let call_id = "local-call" in
  let call_entry_id = History_entry.Id.create ~namespace:"local" ~sequence:0 |> ok in
  let output_entry_id = History_entry.Id.create ~namespace:"local" ~sequence:1 |> ok in
  let outputs =
    [ R.Tool_output.Output.Text "{\"status\":\"完成\"}"
    ; Text (String.make 8192 'x')
    ; Content
        [ Input_text { text = "local multimodal output" }
        ; Input_image { image_url = "data:image/png;base64,opaque"; detail = None }
        ; Input_image { image_url = "https://example.test/high.png"; detail = Some High }
        ; Input_image { image_url = "https://example.test/low.png"; detail = Some Low }
        ; Input_image { image_url = "https://example.test/auto.png"; detail = Some Auto }
        ]
    ]
  in
  List.iter [ P.Call_kind.Function; Custom ] ~f:(fun kind ->
    List.iter [ P.Call_relation.Unresolved; Bound call_entry_id ] ~f:(fun call_relation ->
      List.iter outputs ~f:(fun output ->
        let authored =
          Openai.Responses_history.authored_output ~kind ~call_id ~call_relation ~output
          |> ok
        in
        let restored = P.of_json (P.to_json authored) |> ok in
        (match P.representation restored with
         | Authored -> ()
         | Captured _ | Reconstructed _ -> assert false);
        let semantic = P.semantic restored in
        let metadata = P.Semantic.metadata semantic in
        assert (P.Presence.equal String.equal metadata.call_id (Value call_id));
        List.iter
          [ metadata.item_id; metadata.response_id; metadata.status ]
          ~f:(fun field -> assert (P.Presence.equal String.equal field Absent));
        (match P.Semantic.view semantic with
         | Result { kind = actual_kind; relation; _ } ->
           assert (P.Call_kind.equal kind actual_kind);
           assert (P.Call_relation.equal call_relation relation)
         | Message _ | Call _ | Reasoning _ | Unknown _ -> assert false);
        let entry = History_entry.create_with_id ~id:output_entry_id restored in
        assert (History_entry.Id.equal output_entry_id (History_entry.id entry));
        let expected =
          match kind with
          | P.Call_kind.Function ->
            R.Item.Function_call_output
              { call_id
              ; output
              ; _type = "function_call_output"
              ; id = None
              ; status = None
              }
          | Custom ->
            R.Item.Custom_tool_call_output
              { call_id; output; _type = "custom_tool_call_output"; id = None }
        in
        let lowered = Openai.Responses_history.item_exn entry in
        assert (
          Jsonaf.exactly_equal (R.Item.jsonaf_of_t expected) (R.Item.jsonaf_of_t lowered)))))
;;

let%expect_test "independent neutral capture retains unknown fields and exact bytes" =
  let source =
    {|{"format":"ochat.document","schema_version":1,"kind":"history.payload","future_envelope":{"keep":null},"payload":{"semantic":{"type":"call","kind":"function","name":"inspect","input_bytes":" { \\\"value\\\" : 1 } ","namespace":null,"metadata":{"call_id":"same","item_id":null,"future_metadata":[1,2]},"future_semantic":false},"representation":{"type":"captured","origin":{"type":"unavailable"},"raw":{"type":"future_provider_call","arguments":" { \\\"value\\\" : 1 } ","encrypted_content":"opaque","future":{"unknown":null}}},"future_payload":"retained"}}|}
  in
  let original = Jsonaf.of_string source in
  let restored = P.of_json original |> ok in
  let encoded = P.to_json restored in
  let semantic = P.semantic restored in
  print_s
    [%sexp
      { whole_tree_retained = (Jsonaf.exactly_equal original encoded : bool)
      ; call_bytes =
          ((match P.Semantic.view semantic with
            | Call { input_bytes; _ } -> input_bytes
            | _ -> "unexpected")
           : string)
      ; null_namespace =
          ((match P.Semantic.view semantic with
            | Call { namespace = Null; _ } -> true
            | _ -> false)
           : bool)
      ; unavailable_origin =
          ((match P.representation restored with
            | Captured { origin; _ } -> not (P.Origin.is_available origin)
            | _ -> false)
           : bool)
      }];
  [%expect
    {|
    ((whole_tree_retained true) (call_bytes " { \\\"value\\\" : 1 } ")
     (null_namespace true) (unavailable_origin true))
    |}]
;;

let%expect_test "unknown captured kinds restore independently of runtime lowering" =
  let restored =
    payload
      {|{"format":"ochat.document","schema_version":1,"kind":"history.payload","payload":{"semantic":{"type":"unknown","provider_kind":"new-provider-item","metadata":{}},"representation":{"type":"captured","origin":{"type":"unavailable"},"raw":{"type":"new-provider-item","future":{"text":"opaque"}}}}}|}
  in
  print_s
    [%sexp
      { neutral_valid = (Result.is_ok (P.validate restored) : bool)
      ; legacy_lowering_rejected =
          (Result.is_error (Openai.Responses_history.to_item restored) : bool)
      }];
  [%expect {| ((neutral_valid true) (legacy_lowering_rejected true)) |}]
;;

let%expect_test "neutral admission rejects malformed kind, semantics and duplicate fields"
  =
  let samples =
    [ {|{"format":"ochat.document","schema_version":2,"kind":"history.payload","payload":{}}|}
    ; {|{"format":"ochat.document","schema_version":1,"kind":"history.payload","required_semantics":["new"],"payload":{}}|}
    ; {|{"format":"ochat.document","schema_version":1,"kind":"history.payload","payload":{"semantic":{"type":"call","kind":"function","name":"","input_bytes":"{}","metadata":{}},"representation":{"type":"authored"}}}|}
    ; {|{"format":"ochat.document","schema_version":1,"kind":"history.payload","payload":{"semantic":{"type":"result","kind":"function","relation":{"type":"bound","call_entry_id":"not-a-host-id"},"output":{"type":"text","text":""},"metadata":{}},"representation":{"type":"authored"}}}|}
    ]
  in
  let malformed =
    List.map samples ~f:(fun source ->
      P.of_json (Jsonaf.of_string source) |> Result.is_error)
  in
  let duplicate =
    P.of_json
      (`Object [ "format", `String "ochat.document"; "format", `String "ochat.document" ])
    |> Result.is_error
  in
  print_s [%sexp (malformed : bool list), (duplicate : bool)];
  [%expect {| ((true true true true) true) |}]
;;

let%expect_test "actual wire capture retains opaque reasoning and unknown item envelopes" =
  let origin =
    Openai.Responses_wire.Origin.create
      ~provider:"openai"
      ~account:None
      ~endpoint:"fixture-endpoint"
    |> Result.map_error ~f:(fun _ -> "origin")
    |> ok
  in
  let raw =
    Jsonaf.of_string
      {|{"type":"reasoning","id":"provider-r","summary":[{"type":"summary_text","text":"readable","future_part":9}],"encrypted_content":"opaque","future_item":true}|}
  in
  let wire =
    Openai.Responses_wire.Item.decode raw ~origin
    |> Result.map_error ~f:(fun _ -> "wire")
    |> ok
  in
  let captured = Openai.Responses_history.of_wire_item wire |> ok in
  let restored = P.of_json (P.to_json captured) |> ok in
  print_s
    [%sexp
      { retained_raw =
          ((match P.representation restored with
            | Captured { raw = encoded; _ } -> Jsonaf.exactly_equal raw encoded
            | _ -> false)
           : bool)
      ; readable_summary =
          ((match P.Semantic.view (P.semantic restored) with
            | Reasoning { readable_summary } -> readable_summary
            | _ -> [])
           : string list)
      }];
  [%expect {| ((retained_raw true) (readable_summary (readable))) |}]
;;

let%expect_test
    "standalone unresolved results stay legal and contextual results use host IDs"
  =
  let allocator =
    History_entry.Allocator.create ~namespace:"neutral-pairs" ~next_sequence:0 |> ok
  in
  let call =
    Openai.Responses.Item.Function_call
      { name = "inspect"
      ; arguments = " { } "
      ; call_id = "same"
      ; _type = "function_call"
      ; id = None
      ; status = None
      }
  in
  let result =
    Openai.Responses.Item.Function_call_output
      { call_id = "same"
      ; output = Text "first"
      ; _type = "function_call_output"
      ; id = None
      ; status = None
      }
  in
  let standalone = Openai.Responses_history.create ~allocator result |> ok in
  let entries =
    Openai.Responses_history.of_items
      ~preceding:[]
      ~allocator
      [ call; result; call; result ]
    |> ok
  in
  let selected = List.nth_exn entries 2 in
  let remaining =
    History_entry.remove_with_tool_pair entries ~entry_id:(History_entry.id selected)
    |> ok
  in
  print_s
    [%sexp
      { standalone_unresolved =
          ((match P.Semantic.view (P.semantic (History_entry.payload standalone)) with
            | Result { relation = Unresolved; _ } -> true
            | _ -> false)
           : bool)
      ; canonical_valid =
          (Result.is_ok (History_entry.validate ~allocator (standalone :: entries))
           : bool)
      ; retained_sequences =
          (List.map remaining ~f:(fun entry ->
             History_entry.Id.sequence (History_entry.id entry))
           : int list)
      }];
  [%expect
    {|
    ((standalone_unresolved true) (canonical_valid true)
     (retained_sequences (1 2)))
    |}]
;;

let set_field value ~name ~data =
  match value with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         key, if String.equal key name then data else value))
  | _ -> failwith "test fixture requires object"
;;

let map_field value ~name ~f =
  match value with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         key, if String.equal key name then f value else value))
  | _ -> failwith "test fixture requires object"
;;

let%expect_test "reconstructed DTO cannot execute different neutral semantics" =
  let item =
    Openai.Responses.Item.Function_call
      { name = "inspect"
      ; arguments = "exact"
      ; call_id = "provider"
      ; _type = "function_call"
      ; id = None
      ; status = None
      }
  in
  let original = Openai.Responses_history.of_item item |> ok in
  let changed =
    P.to_json original
    |> map_field
         ~name:"payload"
         ~f:
           (map_field
              ~name:"semantic"
              ~f:(set_field ~name:"name" ~data:(`String "other")))
    |> P.of_json
    |> ok
  in
  print_s
    [%sexp
      { original_runtime_available =
          (Result.is_ok (Openai.Responses_history.to_item original) : bool)
      ; neutral_document_still_valid = (Result.is_ok (P.validate changed) : bool)
      ; mismatched_runtime_rejected =
          (Result.is_error (Openai.Responses_history.to_item changed) : bool)
      }];
  [%expect
    {|
    ((original_runtime_available true) (neutral_document_still_valid true)
     (mismatched_runtime_rejected true))
    |}]
;;

let%expect_test "known capture needs neutral runtime and edits invalidate its replay" =
  let origin =
    Openai.Responses_wire.Origin.create
      ~provider:"openai"
      ~account:None
      ~endpoint:"fixture"
    |> Result.map_error ~f:(fun _ -> "origin")
    |> ok
  in
  let wire =
    Openai.Responses_wire.Item.decode
      (Jsonaf.of_string
         {|{"type":"message","role":"assistant","id":"wire","status":"completed","content":[{"type":"output_text","text":"readable","annotations":[]}],"future":true}|})
      ~origin
    |> Result.map_error ~f:(fun _ -> "wire")
    |> ok
  in
  let captured = Openai.Responses_history.of_wire_item wire |> ok in
  let id = History_entry.Id.create ~namespace:"edited" ~sequence:0 |> ok in
  let entry = History_entry.create_with_id ~id captured in
  let edited =
    Openai.Responses_history.with_item_exn
      entry
      (Openai.Responses.Item.Input_message
         { role = User
         ; content = [ Text { text = "edited"; _type = "input_text" } ]
         ; _type = "message"
         })
  in
  print_s
    [%sexp
      { captured_runtime_rejected =
          (Result.is_error (Openai.Responses_history.to_item captured) : bool)
      ; readable_projection_available =
          (Result.is_ok (Openai.Responses_history.to_presentation_item captured) : bool)
      ; edited_is_authored =
          ((match P.representation (History_entry.payload edited) with
            | Authored -> true
            | Captured _ | Reconstructed _ -> false)
           : bool)
      ; host_identity_retained =
          (History_entry.Id.equal id (History_entry.id edited) : bool)
      }];
  [%expect
    {|
    ((captured_runtime_rejected true) (readable_projection_available true)
     (edited_is_authored true) (host_identity_retained true))
    |}]
;;

let%expect_test
    "explicit occurrence pairing does not follow reused provider call metadata"
  =
  let allocator =
    History_entry.Allocator.create ~namespace:"bound" ~next_sequence:0 |> ok
  in
  let call =
    Openai.Responses.Item.Function_call
      { name = "inspect"
      ; arguments = "{}"
      ; call_id = "same"
      ; _type = "function_call"
      ; id = None
      ; status = None
      }
  in
  let first = Openai.Responses_history.create ~allocator call |> ok in
  let second = Openai.Responses_history.create ~allocator call |> ok in
  let output_id = History_entry.Allocator.allocate allocator |> ok in
  let result =
    Openai.Responses_history.create_with_id_exn
      ~id:output_id
      ~call_relation:(Bound (History_entry.id first))
      (Openai.Responses.Item.Function_call_output
         { call_id = "same"
         ; output = Text "result"
         ; _type = "function_call_output"
         ; id = None
         ; status = None
         })
  in
  let entries = [ first; second; result ] in
  let remaining =
    History_entry.remove_with_tool_pair entries ~entry_id:(History_entry.id first) |> ok
  in
  print_s
    [%sexp
      { explicit_order_valid =
          (Result.is_ok (History_entry.validate ~allocator entries) : bool)
      ; wrong_order_rejected =
          (Result.is_error (History_entry.validate ~allocator [ result; first; second ])
           : bool)
      ; retained_sequences =
          (List.map remaining ~f:(fun entry ->
             History_entry.Id.sequence (History_entry.id entry))
           : int list)
      }];
  [%expect
    {|
    ((explicit_order_valid true) (wrong_order_rejected true)
     (retained_sequences (1)))
    |}]
;;

let%test_unit "opaque neutral JSON uses exact shared numeric and UTF-8 admission" =
  let semantic =
    P.Semantic.create (Unknown { provider_kind = "future" }) ~metadata:P.Metadata.empty
    |> ok
  in
  let capture raw = P.captured semantic ~origin:P.Origin.unavailable ~raw in
  List.iter
    [ ""; " 1 "; "1 2"; "01"; "-01"; "NaN"; "+1"; ".1"; "1."; "1e"; "1e+" ]
    ~f:(fun number ->
      let raw = `Object [ "future", `Number number ] in
      assert (Result.is_error (capture raw));
      assert (Result.is_error (P.reconstructed semantic ~provider:"future" ~raw)));
  List.iter [ "0"; "-0"; "12"; "1e+00"; "1.00"; "-1.25E-2" ] ~f:(fun number ->
    let raw = `Object [ "future", `Number number ] in
    let captured = capture raw |> ok in
    let restored = P.of_json (P.to_json captured) |> ok in
    match P.representation restored with
    | Captured { raw = actual; _ } -> assert (Jsonaf.exactly_equal raw actual)
    | Authored | Reconstructed _ -> assert false);
  List.iter
    [ `String "\255"
    ; `Object [ "\255", `Null ]
    ; `Object [ "same", `Null; "same", `True ]
    ]
    ~f:(fun raw -> assert (Result.is_error (capture raw)));
  let raw = `Object [ "完成", `String "\000\n\"\\💡" ] in
  let captured = capture raw |> ok in
  assert (Result.is_ok (P.validate captured))
;;

let%test_unit "neutral payload JSON preserves prior depth and outer byte policy" =
  let semantic =
    P.Semantic.create (Unknown { provider_kind = "future" }) ~metadata:P.Metadata.empty
    |> ok
  in
  let capture raw = P.captured semantic ~origin:P.Origin.unavailable ~raw in
  let nested count =
    List.init count ~f:Fn.id |> List.fold ~init:`Null ~f:(fun value _ -> `Array [ value ])
  in
  assert (Result.is_ok (capture (nested 125)));
  assert (Result.is_error (capture (nested 126)));
  (* This envelope has fourteen nodes before adding array members. *)
  let members count = `Array (List.init count ~f:(fun _ -> `Null)) in
  assert (Result.is_ok (capture (members 99_986)));
  assert (Result.is_error (capture (members 99_987)));
  (* The canonical payload validator does not impose the generic default byte
     cap. Its actual storage owner performs configured whole-document admission. *)
  assert (Result.is_ok (capture (`String (String.make ((16 * 1024 * 1024) + 1) 'x'))))
;;

let%test_unit "validated semantic decode retains domain and public JSON admission" =
  let semantics : (P.Semantic.view * Jsonaf.t * string) list =
    [ ( Message { form = Output; role = User; content = []; phase = Absent }
      , `Object
          [ "type", `String "message"
          ; "form", `String "output"
          ; "role", `String "user"
          ; "content", `Array []
          ; "metadata", `Object []
          ]
      , "observed output message must have assistant role" )
    ; ( Call
          { kind = Function
          ; name = ""
          ; namespace = Absent
          ; input_bytes = "{}"
          ; async = Absent
          }
      , `Object
          [ "type", `String "call"
          ; "kind", `String "function"
          ; "name", `String ""
          ; "input_bytes", `String "{}"
          ; "metadata", `Object []
          ]
      , "call name must be nonempty" )
    ; ( Unknown { provider_kind = "" }
      , `Object
          [ "type", `String "unknown"
          ; "provider_kind", `String ""
          ; "metadata", `Object []
          ]
      , "unknown provider kind must be nonempty" )
    ]
  in
  List.iter semantics ~f:(fun (view, semantic, expected) ->
    (match P.Semantic.create view ~metadata:P.Metadata.empty with
     | Error actual -> assert (String.equal expected actual)
     | Ok _ -> assert false);
    let document =
      `Object
        [ "format", `String "ochat.document"
        ; "schema_version", `Number "1"
        ; "kind", `String "history.payload"
        ; ( "payload"
          , `Object
              [ "semantic", semantic
              ; "representation", `Object [ "type", `String "authored" ]
              ] )
        ]
    in
    (* Whole JSON is structurally valid; the private semantic path must still reject domain-invalid values. *)
    match P.of_json document with
    | Error actual -> assert (String.equal ("semantic: " ^ expected) actual)
    | Ok _ -> assert false);
  List.iter
    [ `Number "01"; `String "\255" ]
    ~f:(fun raw ->
      assert (
        Result.is_error
          (P.Semantic.create
             (Message
                { form = Input
                ; role = User
                ; phase = Absent
                ; content = [ P.Content.Unknown { kind = "future"; raw } ]
                })
             ~metadata:P.Metadata.empty)))
;;
