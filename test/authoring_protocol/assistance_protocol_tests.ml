open! Core
module P = Agent_protocol
module A = P.Authoring
module H = A.Assistance
module W = A.Wire

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let digest text = Chatml_source.Digest.of_contents text ~checkpoint:Fn.id

let source path contents =
  let id =
    Chatml_source.Id.create ~package_namespace:"assistance-test" ~relative_path:path
    |> Or_error.ok_exn
  in
  A.Source.create
    ~id
    ~digest:(digest contents)
    ~byte_length:(String.length contents)
    ~editor_revision:None
  |> ok
;;

let generator = P.Id.Generator.create ~bytes:(fun length -> String.make length '\000')
let host = P.Id.Server.create_with generator
let context_id = P.Id.Authoring_context.create_with generator
let document = source "draft.chatmd" "<script id=\"s\">"

let context =
  A.Context.create
    ~id:context_id
    ~host
    ~origin:
      (A.Context.Supplied_document { source = document; default_environment = None })
    ~generation:0
    ~basis:(A.Context.Document { source = document; selection = None })
    ~basis_fingerprint:(digest "basis")
    ~editable_sources:[]
  |> ok
;;

let expected = A.Context.Expected.of_view context

let%expect_test "malformed source bytes retain exact lexical owner and scanned prefix" =
  let bytes = "\255abc" in
  let captured = source "malformed.chatml" bytes in
  let request =
    H.Lexical.Request.create ~context:expected ~source:captured ~view:Chatml |> ok
  in
  let owner = H.Lexical.Owner.chatml captured in
  let range = H.Lexical.Range.create ~owner ~start_byte:0 ~end_byte:1 |> ok in
  let span = H.Lexical.Span.create ~range ~role:Identifier in
  let status = H.Lexical.Status.Truncated { next_byte = 2; reason = Work_limit } in
  let result =
    H.Lexical.Result.create ~request ~owner ~spans:[ span ] ~issues:[] ~status |> ok
  in
  let decoded = H.Lexical.Result.of_json (H.Lexical.Result.to_json result) |> ok in
  print_s
    [%sexp
      { invalid_byte_preserved = (A.Source.byte_length captured : int)
      ; span_start = (H.Lexical.Range.start_byte range : int)
      ; span_end = (H.Lexical.Range.end_byte range : int)
      ; decoded_status = (H.Lexical.Result.status decoded : H.Lexical.Status.t)
      }];
  let wire = W.Request.Lexical request in
  (match
     W.Request.of_method_and_params
       ~method_:(W.Request.method_name wire)
       ~params:(W.Request.params wire)
     |> ok
   with
   | W.Request.Lexical value -> assert (H.Lexical.Request.equal value request)
   | _ -> assert false);
  [%expect
    {|
    ((invalid_byte_preserved 4) (span_start 0) (span_end 1)
     (decoded_status (Truncated (next_byte 2) (reason Work_limit))))
    |}]
;;

let%expect_test "lexical byte range and view-affinity model" =
  let captured = source "views.chatmd" "hello" in
  let authored =
    H.Lexical.Owner.chatmd
      ~captured_source:captured
      ~input_identity:(digest "parser-input")
      ~view:Authored
      ~view_sha256:(digest "hello")
      ~view_byte_length:5
    |> ok
  in
  let effective =
    H.Lexical.Owner.chatmd
      ~captured_source:captured
      ~input_identity:(digest "parser-input")
      ~view:Effective
      ~view_sha256:(digest "effective")
      ~view_byte_length:9
    |> ok
  in
  assert (
    Result.is_error
      (H.Lexical.Owner.chatmd
         ~captured_source:captured
         ~input_identity:(digest "parser-input")
         ~view:Authored
         ~view_sha256:(digest "different authored bytes")
         ~view_byte_length:5));
  assert (
    Result.is_error
      (H.Lexical.Owner.chatmd
         ~captured_source:captured
         ~input_identity:(digest "parser-input")
         ~view:Authored
         ~view_sha256:(A.Source.digest captured)
         ~view_byte_length:4));
  for start_byte = 0 to 6 do
    for end_byte = 0 to 6 do
      let actual =
        Result.is_ok (H.Lexical.Range.create ~owner:authored ~start_byte ~end_byte)
      in
      let expected = start_byte < end_byte && end_byte <= 5 in
      assert (Bool.equal actual expected)
    done
  done;
  let request =
    H.Lexical.Request.create ~context:expected ~source:captured ~view:Chatmd_authored
    |> ok
  in
  let good = H.Lexical.Range.create ~owner:authored ~start_byte:0 ~end_byte:1 |> ok in
  let wrong_view =
    H.Lexical.Range.create ~owner:effective ~start_byte:0 ~end_byte:1 |> ok
  in
  let result spans status =
    H.Lexical.Result.create ~request ~owner:authored ~spans ~issues:[] ~status
  in
  assert (
    Result.is_error
      (H.Lexical.Result.create
         ~request
         ~owner:effective
         ~spans:[]
         ~issues:[]
         ~status:Reached_eof));
  assert (
    Result.is_error
      (result [ H.Lexical.Span.create ~range:wrong_view ~role:Raw_text ] Reached_eof));
  assert (
    Result.is_error
      (result
         [ H.Lexical.Span.create ~range:good ~role:Raw_text
         ; H.Lexical.Span.create ~range:good ~role:Identifier
         ]
         Reached_eof));
  assert (
    Result.is_error
      (result
         [ H.Lexical.Span.create ~range:good ~role:Raw_text ]
         (Truncated { next_byte = 0; reason = Span_limit })));
  let module L = Language_highlighting.Highlight_snapshot in
  let roles : (L.Role.t * H.Lexical.Role.t) list =
    [ L.Role.Tag_name, H.Lexical.Role.Tag_name
    ; Attribute_name, Attribute_name
    ; Attribute_value, Attribute_value
    ; Keyword, Keyword
    ; Identifier, Identifier
    ; Type_variable, Type_variable
    ; Variant_tag, Variant_tag
    ; String_literal, String_literal
    ; Number_literal, Number_literal
    ; Boolean_literal, Boolean_literal
    ; Comment, Comment
    ; Raw_delimiter, Raw_delimiter
    ; Raw_text, Raw_text
    ; Script_body, Script_body
    ; Operator, Operator
    ; Punctuation, Punctuation
    ]
  in
  let issues : (L.Issue.kind * H.Lexical.Issue.kind) list =
    [ L.Issue.Open_comment, H.Lexical.Issue.Open_comment
    ; Open_string, Open_string
    ; Open_raw_block, Open_raw_block
    ; Open_tag, Open_tag
    ; Open_script, Open_script
    ]
  in
  assert (List.length roles = 16 && List.length issues = 5);
  List.iter roles ~f:(fun (producer, wire) ->
    assert (Sexp.equal (L.Role.sexp_of_t producer) (H.Lexical.Role.sexp_of_t wire));
    let encoded =
      H.Lexical.Span.create ~range:good ~role:wire |> H.Lexical.Span.to_json
    in
    let decoded = H.Lexical.Span.of_json encoded |> ok in
    assert (H.Lexical.Role.equal (H.Lexical.Span.role decoded) wire));
  List.iter issues ~f:(fun (producer, wire) ->
    assert (Sexp.equal (L.Issue.sexp_of_kind producer) (H.Lexical.Issue.sexp_of_kind wire));
    let encoded =
      H.Lexical.Issue.create ~kind:wire ~range:good |> H.Lexical.Issue.to_json
    in
    let decoded = H.Lexical.Issue.of_json encoded |> ok in
    assert (H.Lexical.Issue.equal_kind (H.Lexical.Issue.kind decoded) wire));
  let large = source "large.chatml" (String.make 8192 'a') in
  let large_request =
    H.Lexical.Request.create ~context:expected ~source:large ~view:Chatml |> ok
  in
  let large_owner = H.Lexical.Owner.chatml large in
  let create count =
    let spans =
      List.init count ~f:(fun start_byte ->
        let range =
          H.Lexical.Range.create ~owner:large_owner ~start_byte ~end_byte:(start_byte + 1)
          |> ok
        in
        H.Lexical.Span.create ~range ~role:Identifier)
    in
    H.Lexical.Result.create
      ~request:large_request
      ~owner:large_owner
      ~spans
      ~issues:[]
      ~status:Reached_eof
  in
  List.iter [ 0; 1; 128; 512 ] ~f:(fun count ->
    let admitted = create count |> ok in
    ignore
      (H.Lexical.Result.of_json (H.Lexical.Result.to_json admitted) |> ok
       : H.Lexical.Result.t));
  assert (Result.is_error (create 8192));
  print_endline "byte/view/prefix, vocabulary and aggregate admission PASS";
  [%expect {| byte/view/prefix, vocabulary and aggregate admission PASS |}]
;;

let%expect_test "seven assistance categories retain truthful evidence" =
  let request =
    H.Assistance.Request.create
      ~context:expected
      ~source:document
      ~cursor_byte:2
      ~max_items:16
    |> ok
  in
  let replace =
    H.Assistance.Range.create ~source:document ~start_byte:1 ~end_byte:3 |> ok
  in
  let selected =
    H.Assistance.Selected_catalog.create
      ~environment:"dev"
      ~input_fingerprint:(digest "admitted-input")
      ~compiler_contract:(digest "selected-contract")
    |> ok
  in
  let root = P.Id.Authoring_root.create_with generator in
  let entries =
    H.Assistance.Suggestion.
      [ Tag, Lexical
      ; Attribute, Parsed_document document
      ; Value, Lexical
      ; Reference, Selected_catalog selected
      ; Path, Host_root root
      ; Template, Installed_builtin { id = "chatml"; sha256 = digest "asset" }
      ; Environment, Parsed_project document
      ]
  in
  let items =
    List.map entries ~f:(fun (kind, evidence) ->
      H.Assistance.Suggestion.create
        ~kind
        ~label:"candidate"
        ~insert_text:"value"
        ~replace
        ~evidence
      |> ok)
  in
  let result = H.Assistance.Result.create ~request ~items ~status:Complete |> ok in
  let decoded = H.Assistance.Result.of_json (H.Assistance.Result.to_json result) |> ok in
  print_s
    [%sexp
      { count = (List.length (H.Assistance.Result.items decoded) : int)
      ; without_build =
          (Option.is_none
             (match A.Context.basis context with
              | A.Context.Document { selection; _ } -> selection
              | _ -> None)
           : bool)
      }];
  let reject kind evidence =
    assert (
      Result.is_error
        (H.Assistance.Suggestion.create
           ~kind
           ~label:"candidate"
           ~insert_text:"value"
           ~replace
           ~evidence))
  in
  reject Environment Lexical;
  reject Path Lexical;
  reject Reference (Installed_builtin { id = "chatml"; sha256 = digest "asset" });
  let forged_root = P.Id.Authoring_root.t_of_sexp (Sexp.Atom "forged") in
  reject Path (Host_root forged_root);
  assert (
    Result.is_error
      (H.Assistance.Result.create ~request ~items ~status:(Unavailable No_local_context)));
  [%expect {| ((count 7) (without_build true)) |}]
;;

let%expect_test "aggregate label admission and strict request JSON" =
  let request =
    H.Assistance.Request.create
      ~context:expected
      ~source:document
      ~cursor_byte:0
      ~max_items:512
    |> ok
  in
  let replace =
    H.Assistance.Range.create ~source:document ~start_byte:0 ~end_byte:0 |> ok
  in
  let item =
    H.Assistance.Suggestion.create
      ~kind:Tag
      ~label:(String.make 4096 'x')
      ~insert_text:""
      ~replace
      ~evidence:Lexical
    |> ok
  in
  let check count =
    H.Assistance.Result.create
      ~request
      ~items:(List.init count ~f:(fun _ -> item))
      ~status:Complete
  in
  List.iter [ 0; 1; 64; 128; 256 ] ~f:(fun count ->
    let result = check count |> ok in
    ignore
      (H.Assistance.Result.of_json (H.Assistance.Result.to_json result) |> ok
       : H.Assistance.Result.t));
  assert (Result.is_error (check 257));
  let fields =
    match H.Assistance.Request.to_json request with
    | `Object fields -> fields
    | _ -> assert false
  in
  assert (
    Result.is_error
      (H.Assistance.Request.of_json (`Object (("max_items", `Number "512") :: fields))));
  assert (
    Result.is_error
      (H.Assistance.Request.of_json (`Object (("unexpected", `True) :: fields))));
  assert (
    Result.is_error
      (H.Assistance.Request.of_json
         (`Object
             (List.map fields ~f:(fun (name, value) ->
                name, if String.equal name "source" then `Null else value)))));
  print_endline "0–256 admitted and decodable; 257 and malformed requests rejected";
  [%expect {| 0–256 admitted and decodable; 257 and malformed requests rejected |}]
;;
