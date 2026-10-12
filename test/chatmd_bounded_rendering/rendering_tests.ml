open! Core
open Fixtures
module R = History_chatmd.Rendering
module P = History_entry.Payload
module H = Agent_protocol.Public.History
module Export = Agent_session.Chatmd_export

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : R.Error.t)]
;;

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let expect_error result ~error =
  match result with
  | Error actual when R.Error.equal actual error -> ()
  | result ->
    raise_s [%sexp "unexpected rendering result", (result : (string, R.Error.t) Result.t)]
;;

let render entries ?(limits = limits ()) () =
  History_chatmd.render_bounded entries ~limits ~checkpoint:ignore
;;

let%expect_test "neutral templates retain host occurrences and exact opaque JSON" =
  let entries = examples () in
  let rendered = render entries () |> ok in
  assert (String.equal rendered (History_chatmd.render entries));
  print_string rendered;
  [%expect
    {|
    <assistant ochat-history-id="1:h:0">
    RAW|
    hello
    |RAW
    </assistant>
    <tool_call function_name="inspect" type="custom_tool_call" namespace="tools" async="true" ochat-history-id="1:h:1">
    RAW|
     { "x" : 1 }
    |RAW
    </tool_call>
    <tool_response type="custom_tool_call" ochat-call-entry-id="1:h:1" ochat-history-id="1:h:2">
    <refusal>RAW|
    no
    |RAW</refusal>
    <img src="urn:image" />
    </tool_response>
    <reasoning ochat-history-id="1:h:3"><summary type="summary_text">RAW|
    a
    |RAW</summary><summary type="summary_text">RAW|
    b
    |RAW</summary></reasoning>
    <ochat-unknown-item kind="future" ochat-history-id="1:h:4">
    RAW|
    {"format":"ochat.document","schema_version":1,"kind":"history.payload","payload":{"semantic":{"type":"unknown","provider_kind":"future","metadata":{}},"representation":{"type":"authored"}}}
    |RAW
    </ochat-unknown-item>
    |}]
;;

let%test_unit "exact quoting and aggregate byte admission use existing encoders" =
  let bytes = String.init 256 ~f:Char.of_int_exn in
  List.iter
    [ ""; bytes; String.make 8192 '\n'; "完成" ]
    ~f:(fun value ->
      let expected = Printf.sprintf "%S" ("prefix:" ^ value ^ ":suffix") in
      let emit output = R.quoted output ~prefix:"prefix:" ~suffix:":suffix" value in
      let actual =
        R.render
          (limits ~max_bytes:(String.length expected) ())
          ~checkpoint:ignore
          ~f:emit
        |> ok
      in
      assert (String.equal actual expected);
      expect_error
        (R.render
           (limits ~max_bytes:(String.length expected - 1) ())
           ~checkpoint:ignore
           ~f:emit)
        ~error:Output_limit);
  let entries = examples () in
  let expected = History_chatmd.render entries in
  assert (
    String.equal
      (render entries ~limits:(limits ~max_bytes:(String.length expected) ()) () |> ok)
      expected);
  assert (
    Result.is_error
      (render entries ~limits:(limits ~max_bytes:(String.length expected - 1) ()) ()))
;;

let%test_unit "JSON measurement shares the pinned serializer without new legacy policy" =
  let ascii = String.init 128 ~f:Char.of_int_exn in
  let samples =
    [ `Null
    ; `True
    ; `False
    ; `Number "1e2"
    ; `String ascii
    ; `String "完成"
    ; `Array [ `Null; `String "\\\"\n" ]
    ; `Object [ "key\n", `String (String.make 8192 'x'); "nested", `Array [ `True ] ]
    ]
  in
  List.iter samples ~f:(fun value ->
    let expected = Jsonaf.to_string value in
    let emit output = R.json output value in
    let rendered =
      R.render (limits ~max_bytes:(String.length expected) ()) ~checkpoint:ignore ~f:emit
      |> ok
    in
    assert (String.equal rendered expected);
    assert (
      Result.is_error
        (R.render
           (limits ~max_bytes:(String.length expected - 1) ())
           ~checkpoint:ignore
           ~f:emit)));
  let malformed = `Number "01" in
  assert (
    String.equal (R.render_unbounded ~f:(fun output -> R.json output malformed)) "01");
  assert (
    Result.is_error
      (R.render (limits ()) ~checkpoint:ignore ~f:(fun output -> R.json output malformed)));
  List.iter
    [ 1, 100, 100; 100, 1, 100; 100, 100, 1 ]
    ~f:(fun (max_depth, max_fields, max_nodes) ->
      let json_limits =
        Document_schema.Limits.create ~max_bytes:1000 ~max_depth ~max_fields ~max_nodes
        |> function
        | Ok value -> value
        | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
      in
      assert (
        Result.is_error
          (R.render (limits ~json_limits ()) ~checkpoint:ignore ~f:(fun output ->
             R.json output (`Object [ "a", `Array [ `True; `False ]; "b", `Null ])))))
;;

let%test_unit
    "work limits, rejected staging and escaped owners cannot return partial output"
  =
  let emit output = List.iter (List.init 100 ~f:ignore) ~f:(fun () -> R.text output "") in
  expect_error
    (R.render (limits ~max_work_steps:10 ()) ~checkpoint:ignore ~f:emit)
    ~error:Work_limit;
  expect_error
    (R.render (limits ~max_bytes:1 ()) ~checkpoint:ignore ~f:(fun output ->
       ignore (Option.try_with (fun () -> R.text output "too large") : unit option)))
    ~error:Output_limit;
  let escaped = ref None in
  ignore
    (R.render (limits ()) ~checkpoint:ignore ~f:(fun output -> escaped := Some output)
     |> ok
     : string);
  assert (
    match R.text (Option.value_exn !escaped) "late" with
    | () -> false
    | exception Invalid_argument _ -> true);
  ignore
    (R.render (limits ()) ~checkpoint:ignore ~f:(fun output ->
       let foreign =
         Stdlib.Domain.spawn (fun () ->
           match R.text output "foreign" with
           | () -> false
           | exception Invalid_argument _ -> true)
       in
       assert (Stdlib.Domain.join foreign))
     |> ok
     : string);
  assert (
    Result.is_error
      (R.Limits.create
         ~max_bytes:0
         ~max_work_steps:1
         ~json_limits:Document_schema.Limits.default))
;;

let%expect_test "public disclosure and provenance use the same admitted output" =
  let source = List.hd_exn (examples ()) in
  let visible =
    H.Visible.of_semantic (P.semantic (History_entry.payload source)) |> Option.value_exn
  in
  let visible = H.visible (id 0) ~provenance:Canonical visible |> protocol_ok in
  let redacted =
    H.redacted
      (id 1)
      ~provenance:Canonical
      (H.Redaction.create ~disclosed_header:(Some (Unknown "future")))
    |> protocol_ok
  in
  let delivery = Agent_protocol.Id.Delivery.of_string "dlv_render" |> protocol_ok in
  let notified =
    H.full source ~provenance:(Runtime_notification delivery) |> protocol_ok
  in
  let entries = [ visible; redacted; notified ] in
  let expected = Export.render_public entries in
  let bounded =
    Export.render_public_bounded
      entries
      ~limits:(limits ~max_bytes:(String.length expected) ())
      ~checkpoint:ignore
    |> ok
  in
  assert (String.equal bounded expected);
  assert (
    Result.is_error
      (Export.render_public_bounded
         entries
         ~limits:(limits ~max_bytes:(String.length expected - 1) ())
         ~checkpoint:ignore));
  print_string bounded;
  [%expect
    {|
    <ochat-public-view disclosure="visible" role="assistant" ochat-history-id="1:h:0">
    RAW|
    hello
    |RAW
    </ochat-public-view>
    <ochat-public-view disclosure="redacted" role="unknown future" ochat-history-id="1:h:1">
    [Content redacted]
    </ochat-public-view>
    <!-- ochat-runtime-notification delivery_id="dlv_render" -->
    <assistant ochat-history-id="1:h:0">
    RAW|
    hello
    |RAW
    </assistant>
    |}]
;;

let%test_unit
    "large identifiers and authoring projection participate in aggregate admission"
  =
  let source = List.hd_exn (examples ()) in
  let large_id =
    History_entry.Id.create ~namespace:(String.make 8192 'x') ~sequence:42
    |> Result.ok_or_failwith
  in
  let large = History_entry.create_with_id ~id:large_id (History_entry.payload source) in
  assert (Result.is_error (render [ large ] ~limits:(limits ~max_bytes:128 ()) ()));
  let expected = Printf.sprintf "%S" (History_entry.Id.to_string large_id) in
  assert (
    String.equal
      (R.render
         (limits ~max_bytes:(String.length expected) ())
         ~checkpoint:ignore
         ~f:(fun output -> R.quoted_history_id output large_id)
       |> ok)
      expected);
  let hash = String.make 64 'a' in
  let guidance =
    Agent_protocol.Authoring_guidance.create
      ~context_identity:hash
      ~policy_fingerprint:hash
      ~purpose:Reference
      ~topics:
        [ { id = "chatml.tasks"
          ; document_sha256 = hash
          ; source = Installed hash
          ; complete = true
          }
        ]
      ~payload:(P.to_json (History_entry.payload source))
    |> protocol_ok
  in
  let entry = H.full source ~provenance:(Runtime_authoring guidance) |> protocol_ok in
  let expected = Export.render_public [ entry ] in
  assert (
    String.equal
      (Export.render_public_bounded
         [ entry ]
         ~limits:(limits ~max_bytes:(String.length expected) ())
         ~checkpoint:ignore
       |> ok)
      expected);
  assert (
    Result.is_error
      (Export.render_public_bounded
         [ entry ]
         ~limits:(limits ~max_work_steps:8 ())
         ~checkpoint:ignore))
;;

let%test_unit
    "every observed checkpoint preserves callback identity and final cancellation"
  =
  let entries = examples () |> List.map ~f:full in
  let calls = ref 0 in
  ignore
    (Export.render_public_bounded entries ~limits:(limits ()) ~checkpoint:(fun () ->
       incr calls)
     |> ok
     : string);
  assert (!calls > 0);
  for failure_at = 1 to !calls do
    let sentinel = Failure "render-checkpoint" in
    let seen = ref 0 in
    let checkpoint () =
      incr seen;
      if !seen = failure_at then raise sentinel
    in
    match Export.render_public_bounded entries ~limits:(limits ()) ~checkpoint with
    | _ -> assert false
    | exception exn -> assert (phys_equal exn sentinel)
  done;
  Eio_main.run (fun _ ->
    let delivered = ref false in
    let sentinel = Failure "render-cancel" in
    match
      Eio.Cancel.sub (fun context ->
        let seen = ref 0 in
        let checkpoint () =
          incr seen;
          if !seen = !calls then Eio.Cancel.cancel context sentinel;
          Eio.Fiber.check ()
        in
        ignore
          (Export.render_public_bounded entries ~limits:(limits ()) ~checkpoint |> ok
           : string);
        delivered := true)
    with
    | () -> assert false
    | exception Eio.Cancel.Cancelled reason ->
      assert (phys_equal reason sentinel);
      assert (not !delivered))
;;
