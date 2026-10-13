open Core
module S = Chatml_source
module B = Chatml_project_build
module C = Chatml_authoring_context
module F = Chatml_authoring_fallback
module FD = Chatmd_authoring_fallback
module D = Chatmd_authoring_analysis
module P = Agent_protocol
module W = P.Authoring.Assistance.Assistance
module CM = Prompt.Chat_markdown

let wire_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (P.Error.sexp_of_t error)
;;

let fallback_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (F.Error.sexp_of_t error)
;;

let source contents = Support.capture ~namespace:"fallback" "main.chatml" contents

let wire_source source =
  P.Authoring.Source.create
    ~id:(S.id source)
    ~digest:(S.digest source)
    ~byte_length:(S.byte_length source)
    ~editor_revision:None
  |> wire_ok
;;

let generator = P.Id.Generator.create ~bytes:(fun length -> String.make length '\000')
let host = P.Id.Server.create_with generator
let context_id = P.Id.Authoring_context.create_with generator
let bundle_id = P.Id.Authoring_bundle.create_with generator

let expected () =
  let digest = S.Digest.of_contents "fixture" ~checkpoint:ignore in
  P.Authoring.Context.create
    ~id:context_id
    ~host
    ~origin:(P.Authoring.Context.Uploaded_project bundle_id)
    ~generation:0
    ~basis:
      (P.Authoring.Context.Project
         { base_graph = digest
         ; selection = { environment = "fixture"; compiler_contract = digest }
         })
    ~basis_fingerprint:digest
    ~editable_sources:[]
  |> wire_ok
  |> P.Authoring.Context.Expected.of_view
;;

let request source ~cursor_byte ~max_items =
  W.Request.create
    ~context:(expected ())
    ~source:(wire_source source)
    ~cursor_byte
    ~max_items
  |> wire_ok
;;

let limits
      ?(projection = 100_000)
      ?(contract_identity = 20_000_000)
      ?(contract_work = 100_000)
      ?(output = 200_000)
      ()
  =
  F.Limits.create
    ~lexical:
      (Language_highlighting.Highlight_snapshot.Limits.create
         ~max_source_bytes:100_000
         ~max_identity_bytes:2048
         ~max_spans:10_000
         ~max_issues:256
         ~max_work_units:100_000
         ~max_nesting:128
       |> Or_error.ok_exn)
    ~max_contract_identity_bytes:contract_identity
    ~max_contract_work_steps:contract_work
    ~max_projection_work_steps:projection
    ~max_output_bytes:output
  |> Or_error.ok_exn
;;

let items result = W.Result.items result
let names result = List.map (items result) ~f:W.Suggestion.label
let has_name result name = List.mem (names result) name ~equal:String.equal

let produce ?(selected = None) ?(limits = limits ()) source ~cursor_byte ~max_items =
  let request = request source ~cursor_byte ~max_items in
  F.produce (F.Origin.chatml source) ~request ~selected ~limits ~checkpoint:ignore
;;

let assert_roundtrip result =
  let decoded = W.Result.of_json (W.Result.to_json result) |> wire_ok in
  assert (W.Request.equal (W.Result.request decoded) (W.Result.request result))
;;

let status_name = function
  | W.Status.Complete -> "complete"
  | Truncated _ -> "truncated"
  | Unavailable _ -> "unavailable"
;;

let%expect_test "lexical fallback is exact captured syntax without a Build" =
  let source = source "let value = le" in
  let request = request source ~cursor_byte:(S.byte_length source) ~max_items:64 in
  let result =
    F.produce
      (F.Origin.chatml source)
      ~request
      ~selected:None
      ~limits:(limits ())
      ~checkpoint:ignore
    |> fallback_ok
  in
  assert_roundtrip result;
  print_s
    [%sexp
      { status = (status_name (W.Result.status result) : string)
      ; names = (names result : string list)
      ; replacement =
          (List.map (items result) ~f:(fun item ->
             let range = W.Suggestion.replace item in
             W.Range.start_byte range, W.Range.end_byte range)
           : (int * int) list)
      }];
  [%expect {| ((status complete) (names (let)) (replacement ((12 14)))) |}]
;;

let%expect_test "selected contract is one actual target, including failed source" =
  let fixture = Fixtures.make () in
  let build = Fixtures.compose fixture |> Support.ok in
  Eio_main.run (fun env ->
    match
      Chatml_compilation.compile_prepared
        (B.prepared build)
        ~limits:Chatml_compilation.default_limits
        ~max_dependency_depth:16
        ~env
        ~target:One_off_v1
        ()
    with
    | Error _ -> ()
    | Ok _ -> failwith "fixture unexpectedly passed the selected compiler check");
  let selected =
    F.Selected_contract.admit
      build
      ~selected_contract:fixture.contract
      ~limits:(limits ())
      ~checkpoint:ignore
    |> fallback_ok
  in
  let source = fixture.main in
  let request = request source ~cursor_byte:0 ~max_items:512 in
  let result =
    F.produce
      (F.Origin.chatml source)
      ~request
      ~selected:(Some selected)
      ~limits:(limits ())
      ~checkpoint:ignore
    |> fallback_ok
  in
  assert_roundtrip result;
  print_s
    [%sexp
      { has_print = (has_name result "print" : bool)
      ; has_job = (has_name result "Job" : bool)
      ; has_let = (has_name result "let" : bool)
      ; status = (status_name (W.Result.status result) : string)
      }];
  [%expect {| ((has_print false) (has_job true) (has_let true) (status complete)) |}]
;;

let%test_unit "selected contract and captured source are exact, bounded evidence" =
  let module Contract = Chatml.Chatml_compiler_contract in
  let fixture = Fixtures.make () in
  let build = Fixtures.compose fixture |> Support.ok in
  let selected =
    F.Selected_contract.admit
      build
      ~selected_contract:fixture.contract
      ~limits:(limits ())
      ~checkpoint:ignore
    |> fallback_ok
  in
  let wrong_target = Contract.for_target Moderator_v1 ~checkpoint:ignore in
  (match
     F.Selected_contract.admit
       build
       ~selected_contract:wrong_target
       ~limits:(limits ())
       ~checkpoint:ignore
   with
   | Error F.Error.Contract_mismatch -> ()
   | Ok _ | Error _ -> assert false);
  let moderator_fixture = Fixtures.make ~contract:wrong_target () in
  let moderator_build = Fixtures.compose moderator_fixture |> Support.ok in
  let moderator =
    F.Selected_contract.admit
      moderator_build
      ~selected_contract:wrong_target
      ~limits:(limits ())
      ~checkpoint:ignore
    |> fallback_ok
  in
  let moderator_result =
    produce
      ~selected:(Some moderator)
      moderator_fixture.main
      ~cursor_byte:0
      ~max_items:512
    |> fallback_ok
  in
  assert (has_name moderator_result "Invocation");
  let one_off_result =
    produce ~selected:(Some selected) fixture.main ~cursor_byte:0 ~max_items:512
    |> fallback_ok
  in
  assert (not (has_name one_off_result "Invocation"));
  (match
     F.Selected_contract.admit
       build
       ~selected_contract:fixture.contract
       ~limits:(limits ~contract_identity:1 ())
       ~checkpoint:ignore
   with
   | Error F.Error.Contract_identity_limit -> ()
   | Ok _ | Error _ -> assert false);
  (match
     F.Selected_contract.admit
       build
       ~selected_contract:fixture.contract
       ~limits:(limits ~contract_work:1 ())
       ~checkpoint:ignore
   with
   | Error F.Error.Contract_work_limit -> ()
   | Ok _ | Error _ -> assert false);
  let unrelated = source "le" in
  let unrelated_request = request unrelated ~cursor_byte:2 ~max_items:64 in
  (match
     F.produce
       (F.Origin.chatml unrelated)
       ~request:unrelated_request
       ~selected:(Some selected)
       ~limits:(limits ())
       ~checkpoint:ignore
   with
   | Error F.Error.Stale_selected_build -> ()
   | Ok _ | Error _ -> assert false);
  match
    F.produce
      (F.Origin.chatml fixture.main)
      ~request:unrelated_request
      ~selected:None
      ~limits:(limits ())
      ~checkpoint:ignore
  with
  | Error F.Error.Stale_request_source -> ()
  | Ok _ | Error _ -> assert false
;;

let%test_unit "every successful status respects the encoded envelope cap" =
  let check source ~cursor_byte ~max_items ~origin ~expected_status ~empty =
    let request = request source ~cursor_byte ~max_items in
    let full =
      F.produce origin ~request ~selected:None ~limits:(limits ()) ~checkpoint:ignore
      |> fallback_ok
    in
    assert (W.Status.equal (W.Result.status full) expected_status);
    let encoded = W.Result.to_json full |> Jsonaf.to_string |> String.length in
    (match
       F.produce
         origin
         ~request
         ~selected:None
         ~limits:(limits ~output:1 ())
         ~checkpoint:ignore
     with
     | Error F.Error.Output_byte_limit -> ()
     | Ok _ | Error _ -> assert false);
    let exact =
      F.produce
        origin
        ~request
        ~selected:None
        ~limits:(limits ~output:encoded ())
        ~checkpoint:ignore
    in
    (match exact with
     | Ok result ->
       let actual = W.Result.to_json result |> Jsonaf.to_string |> String.length in
       assert (actual <= encoded);
       if empty then assert (W.Status.equal (W.Result.status result) expected_status)
     | Error F.Error.Output_byte_limit when not empty -> ()
     | Error _ -> assert false);
    match
      F.produce
        origin
        ~request
        ~selected:None
        ~limits:(limits ~output:(encoded - 1) ())
        ~checkpoint:ignore
    with
    | Ok result when not empty ->
      let actual = W.Result.to_json result |> Jsonaf.to_string |> String.length in
      assert (actual < encoded)
    | Ok _ -> assert false
    | Error F.Error.Output_byte_limit -> ()
    | Error _ -> assert false
  in
  let code = source "let x = le" in
  check
    code
    ~cursor_byte:(S.byte_length code)
    ~max_items:64
    ~origin:(F.Origin.chatml code)
    ~expected_status:W.Status.Complete
    ~empty:false;
  let comment = source "(* held *)" in
  check
    comment
    ~cursor_byte:4
    ~max_items:64
    ~origin:(F.Origin.chatml comment)
    ~expected_status:W.Status.Complete
    ~empty:true;
  let capped = source "" in
  check
    capped
    ~cursor_byte:0
    ~max_items:1
    ~origin:(F.Origin.chatml capped)
    ~expected_status:(W.Status.Truncated W.Status.Item_limit)
    ~empty:false;
  let unavailable = source "x" in
  let map =
    F.Site_map.
      { compiled_point = (fun _ ~checkpoint:_ -> Ok None)
      ; authored_replacement = (fun _ ~checkpoint:_ -> assert false)
      }
  in
  check
    unavailable
    ~cursor_byte:0
    ~max_items:1
    ~origin:(F.Origin.mapped ~compiled:unavailable ~authored:unavailable ~map)
    ~expected_status:(W.Status.Unavailable W.Status.Source_map_unavailable)
    ~empty:true
;;

let%test_unit "ignored lexical token interiors do not emit identifier suggestions" =
  List.iter
    [ "let x = 123", 9
    ; "let x = true", 9
    ; "let x = \"abc\"", 10
    ; "(* hello *)", 4
    ; "'result", 3
    ; "`Some", 3
    ; "(* open", 7
    ; "\"open", 5
    ]
    ~f:(fun (text, cursor_byte) ->
      let source = source text in
      let result = produce source ~cursor_byte ~max_items:64 |> fallback_ok in
      assert (List.is_empty (items result));
      assert (W.Status.equal (W.Result.status result) W.Status.Complete));
  List.iter
    [ "le()", 2; "le\"x\"", 2 ]
    ~f:(fun (text, cursor_byte) ->
      let source = source text in
      let result = produce source ~cursor_byte ~max_items:64 |> fallback_ok in
      assert (has_name result "let"))
;;

let%test_unit "static syntax templates use production keyword and tuple tokens" =
  let module T = Chatml.Chatml_tokens in
  let token text = Chatml.Chatml_lexer.token (Lexing.from_string text) in
  List.iter
    [ ( "let"
      , function
        | T.LET -> true
        | _ -> false )
    ; ( "fun"
      , function
        | T.FUN -> true
        | _ -> false )
    ; ( "if"
      , function
        | T.IF -> true
        | _ -> false )
    ; ( "match"
      , function
        | T.MATCH -> true
        | _ -> false )
    ; ( "module"
      , function
        | T.MODULE -> true
        | _ -> false )
    ; ( "type"
      , function
        | T.TYPE -> true
        | _ -> false )
    ]
    ~f:(fun (text, matches) -> assert (matches (token text)));
  let lexer = Lexing.from_string "(| (), () |)" in
  let rec collect n acc =
    if Int.equal n 0
    then List.rev acc
    else collect (n - 1) (Chatml.Chatml_lexer.token lexer :: acc)
  in
  let tuple_tokens = collect 8 [] in
  (match tuple_tokens with
   | [ T.TUPLE_OPEN
     ; T.LPAREN
     ; T.RPAREN
     ; T.COMMA
     ; T.LPAREN
     ; T.RPAREN
     ; T.TUPLE_CLOSE
     ; T.EOF
     ] -> ()
   | _ -> assert false);
  let empty = source "" in
  let result = produce empty ~cursor_byte:0 ~max_items:64 |> fallback_ok in
  assert (
    List.equal
      String.equal
      (names result)
      [ "let"; "fun"; "if"; "match"; "module"; "type"; "()"; "[]"; "{}"; "(| (), () |)" ])
;;

exception Interrupted of unit ref

let%test_unit "late work cap and checkpoint preserve partial status or exception identity"
  =
  let captured = source "let value = le" in
  let request = request captured ~cursor_byte:(S.byte_length captured) ~max_items:64 in
  let polls = ref 0 in
  let complete =
    F.produce
      (F.Origin.chatml captured)
      ~request
      ~selected:None
      ~limits:(limits ())
      ~checkpoint:(fun () -> Int.incr polls)
    |> fallback_ok
  in
  assert (W.Status.equal (W.Result.status complete) W.Status.Complete);
  assert (!polls > 10);
  (match
     F.produce
       (F.Origin.chatml captured)
       ~request
       ~selected:None
       ~limits:(limits ~projection:(!polls - 1) ())
       ~checkpoint:ignore
   with
   | Ok result ->
     assert (
       W.Status.equal (W.Result.status result) (W.Status.Truncated W.Status.Work_limit));
     assert_roundtrip result
   | Error _ -> assert false);
  let token = ref () in
  let interruption = Interrupted token in
  let counter = ref 0 in
  (match
     F.produce
       (F.Origin.chatml captured)
       ~request
       ~selected:None
       ~limits:(limits ())
       ~checkpoint:(fun () ->
         Int.incr counter;
         if !counter >= !polls - 1 then raise interruption)
   with
   | _ -> assert false
   | exception actual -> assert (phys_equal actual interruption));
  let invalid_utf8 = source "let x = \255" in
  let result = produce invalid_utf8 ~cursor_byte:0 ~max_items:64 |> fallback_ok in
  assert_roundtrip result
;;
