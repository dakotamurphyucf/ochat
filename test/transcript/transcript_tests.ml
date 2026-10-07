open! Core
module T = Transcript
module P = History_entry.Payload

let ok = Result.ok_or_failwith
let document_limits = T.Admission.default

let scope source attempt relation =
  T.Scope.create
    ~source:(T.Source_id.of_string source |> ok)
    ~attempt:(T.Attempt_id.of_string attempt |> ok)
    ~relation
  |> ok
;;

let root = scope "root" "attempt-1" Root

let descriptor ?header ?entry_id ?call_name scope id =
  T.Item.create ~scope ~id:(T.Item_id.of_string id |> ok) ~entry_id ~header ~call_name
  |> ok
;;

let part descriptor index kind =
  T.Part.create
    ~item:descriptor
    ~id:(T.Part_id.of_string (Int.to_string index) |> ok)
    ~index:(Some index)
    ~kind
  |> ok
;;

let event view = T.Stream.create view ~limits:document_limits |> ok

let draft ?(bytes = 65536) () =
  let limits =
    T.Draft.Limits.create
      ~max_scopes:8
      ~max_items:16
      ~max_parts:32
      ~max_unknown_events:4
      ~max_retained_bytes:bytes
      ~document_limits
    |> ok
  in
  T.Draft.create ~limits
;;

let apply draft view = T.Draft.apply draft (event view) |> ok |> fst

let text_of draft =
  match T.Draft.items draft with
  | [ { state = Partial { parts = [ { text = Some text; _ } ]; _ }; _ } ] -> text
  | _ -> failwith "expected one text draft"
;;

let message_payload role text =
  let semantic =
    P.Semantic.create
      (Message
         { form =
             (match role with
              | P.Role.Assistant -> P.Semantic.Output
              | System | Developer | User | Tool -> Input)
         ; role
         ; content = [ Text { text; annotations = []; logprobs = Absent } ]
         ; phase = Absent
         })
      ~metadata:P.Metadata.empty
    |> ok
  in
  P.authored semantic
;;

let entry payload sequence =
  let id = History_entry.Id.create ~namespace:"transcript-test" ~sequence |> ok in
  History_entry.create_with_id ~id payload
;;

let%expect_test "orphan prefixes, exact replacement, scoped parts and Developer" =
  let item = descriptor root "item" in
  let part = part item 3 Text in
  let first =
    apply (draft ()) (Changed { target = Content part; change = Append "suffix" })
  in
  let text = text_of first in
  print_s
    [%sexp
      (text.value : string)
    , ((match text.completeness with
        | Missing_prefix -> true
        | Prefix_observed -> false)
       : bool)];
  let second =
    apply first (Changed { target = Content part; change = Replace "complete" })
  in
  let text = text_of second in
  print_s
    [%sexp
      (text.value : string)
    , ((match text.completeness with
        | Missing_prefix -> true
        | Prefix_observed -> false)
       : bool)];
  print_s
    (T.Header.sexp_of_t
       (T.Header.of_semantic (P.semantic (message_payload Developer "instruction"))));
  [%expect
    {|
    (suffix true)
    (complete false)
    (Message Developer)
    |}]
;;

let%test_unit "candidate budget rejects atomically before append and counts escaped bytes"
  =
  let item = descriptor ~header:(Message Assistant) root "item" in
  let content = part item 1 Text in
  let original = apply (draft ()) (Part_announced content) in
  let original =
    apply original (Changed { target = Content content; change = Append "\n\"\\é" })
  in
  let exact = T.Draft.retained_bytes original in
  assert (exact > String.length (text_of original).value);
  let denied =
    T.Draft.apply
      original
      ~max_retained_bytes:exact
      (event
         (Changed { target = Content content; change = Append (String.make 8192 'x') }))
  in
  assert (Result.is_error denied);
  assert (String.equal (text_of original).value "\n\"\\é");
  assert (T.Draft.retained_bytes original = exact);
  let cleared = T.Draft.remove_item original (T.Item.key item) in
  assert (List.is_empty (T.Draft.items cleared));
  assert (T.Draft.retained_bytes cleared < exact);
  assert (T.Draft.retained_bytes (T.Draft.clear_scope cleared root.key) = 0)
;;

let%test_unit "nested items and attempts are isolated; finalized payload remains exact" =
  let nested =
    scope
      "child"
      "attempt-1"
      (Nested { scope = root.key; call_entry_id = None; call_alias = Some "parent-call" })
  in
  let next = scope "root" "attempt-2" Root in
  let original =
    List.fold [ root; nested; next ] ~init:(draft ()) ~f:(fun state scope ->
      let item = descriptor scope "same-provider-alias" in
      let content = part item 0 Text in
      apply state (Changed { target = Content content; change = Append "partial" }))
  in
  assert (List.length (T.Draft.items original) = 3);
  let payload = P.to_json (message_payload Assistant "done") in
  let payload =
    match payload with
    | `Object fields -> `Object (fields @ [ "future_envelope", `Number "1e+00" ])
    | _ -> assert false
  in
  let payload = P.of_json payload |> ok in
  let entry = entry payload 0 in
  let descriptor =
    descriptor
      ~entry_id:(History_entry.id entry)
      ~header:(Message Assistant)
      root
      "same-provider-alias"
  in
  let finalized = apply original (Item_finalized { item = descriptor; entry }) in
  let actual =
    List.find_exn (T.Draft.items finalized) ~f:(fun item ->
      T.Item.Key.equal (T.Item.key item.descriptor) (T.Item.key descriptor))
  in
  (match actual.state with
   | Finalized actual ->
     assert (
       Jsonaf.exactly_equal (P.to_json payload) (P.to_json (History_entry.payload actual)))
   | Partial _ -> assert false);
  assert (
    Result.is_error
      (T.Draft.apply
         finalized
         (event
            (Changed { target = Content (part descriptor 0 Text); change = Append "late" }))));
  assert (List.length (T.Draft.items (T.Draft.clear_scope finalized root.key)) = 2)
;;

let%test_unit "refinement, gaps, unknown envelopes and source terminal fences" =
  let item = descriptor root "item" in
  let content = part item 0 Text in
  let state = apply (draft ()) (Part_announced content) in
  let contradiction = descriptor ~header:(Call Function) root "item" in
  assert (Result.is_error (T.Draft.apply state (event (Item_announced contradiction))));
  let state = T.Draft.mark_gap state ~scope:(Some root.key) in
  let state =
    apply state (Changed { target = Content content; change = Append "later" })
  in
  (match (text_of state).completeness with
   | Missing_prefix -> ()
   | Prefix_observed -> assert false);
  let json =
    T.Stream.to_json
      (event
         (Unknown_event
            { scope = root
            ; provider_kind = "future"
            ; raw = `Object [ "opaque", `Number "1.00" ]
            }))
  in
  let json =
    match json with
    | `Object fields -> `Object (fields @ [ "future_transport", `String "retained" ])
    | _ -> assert false
  in
  let decoded = T.Stream.of_json json ~limits:document_limits |> ok in
  assert (Jsonaf.exactly_equal json (T.Stream.to_json decoded));
  assert (T.Stream.encoded_bytes decoded = String.length (Jsonaf.to_string json));
  let state = T.Draft.apply state decoded |> ok |> fst in
  let state = apply state (Source_finished { scope = root; completion = Complete }) in
  assert (Result.is_error (T.Draft.apply state (event (Item_announced item))));
  assert (List.length (T.Draft.unknown_events state) = 1)
;;

let%test_unit "provider done is provisional; orphan deltas have no guessed role" =
  let module R = Openai.Responses.Response_stream in
  let adapter = Openai.Responses_live.create ~scope:root ~limits:document_limits in
  let adapter, events =
    Openai.Responses_live.observe_legacy
      adapter
      ~entry_id:None
      (R.Output_text_delta
         { content_index = 4
         ; delta = "suffix"
         ; item_id = "provider-item"
         ; output_index = 2
         ; type_ = "response.output_text.delta"
         })
    |> ok
  in
  let state =
    List.fold events ~init:(draft ()) ~f:(fun state event ->
      T.Draft.apply state event |> ok |> fst)
  in
  assert (Option.is_none (List.hd_exn (T.Draft.items state)).descriptor.header);
  (match (text_of state).completeness with
   | Missing_prefix -> ()
   | Prefix_observed -> assert false);
  let adapter, events =
    Openai.Responses_live.observe_legacy
      adapter
      ~entry_id:None
      (R.Output_text_done
         { content_index = 4
         ; text = "complete"
         ; item_id = "provider-item"
         ; output_index = 2
         ; type_ = "response.output_text.done"
         })
    |> ok
  in
  assert (
    List.for_all events ~f:(fun event ->
      match T.Stream.view event with
      | Item_finalized _ -> false
      | _ -> true));
  let payload = message_payload Assistant "complete" in
  let entry = entry payload 1 in
  let _, events = Openai.Responses_live.finalized adapter entry |> ok in
  assert (
    List.exists events ~f:(fun event ->
      match T.Stream.view event with
      | Item_finalized { entry = actual; _ } ->
        History_entry.Id.equal (History_entry.id actual) (History_entry.id entry)
      | _ -> false))
;;

let%test_unit "committed preparation can replace provisional call name and header" =
  let module R = Openai.Responses in
  let call name =
    P.Semantic.create
      (Call
         { kind = Function
         ; name
         ; namespace = Absent
         ; input_bytes = "edited-input"
         ; async = Absent
         })
      ~metadata:P.Metadata.empty
    |> ok
    |> P.authored
  in
  List.iter
    [ call "redirected"; message_payload Developer "prepared instruction" ]
    ~f:(fun payload ->
      let committed = entry payload 10 in
      let host = History_entry.id committed in
      let provider_call : R.Function_call.t =
        { name = "original"
        ; arguments = "original-input"
        ; call_id = "call"
        ; _type = "function_call"
        ; id = Some "provider-call"
        ; status = None
        }
      in
      let adapter = Openai.Responses_live.create ~scope:root ~limits:document_limits in
      let adapter, initial =
        Openai.Responses_live.observe_legacy
          adapter
          ~entry_id:(Some host)
          (R.Response_stream.Output_item_added
             { item = Function_call provider_call
             ; output_index = 0
             ; type_ = "response.output_item.added"
             })
        |> ok
      in
      let state =
        List.fold initial ~init:(draft ()) ~f:(fun state event ->
          T.Draft.apply state event |> ok |> fst)
      in
      let descriptor =
        descriptor
          ~entry_id:host
          ~header:(Call Function)
          ~call_name:"different"
          root
          "provider-call"
      in
      assert (Result.is_error (T.Draft.apply state (event (Item_announced descriptor))));
      let adapter, finalized = Openai.Responses_live.finalized adapter committed |> ok in
      let state =
        List.fold finalized ~init:state ~f:(fun state event ->
          T.Draft.apply state event |> ok |> fst)
      in
      let actual = List.hd_exn (T.Draft.items state) in
      assert (
        Option.equal
          T.Header.equal
          actual.descriptor.header
          (Some (T.Header.of_semantic (P.semantic payload))));
      (match actual.state with
       | Finalized actual ->
         assert (
           Jsonaf.exactly_equal
             (P.to_json payload)
             (P.to_json (History_entry.payload actual)))
       | Partial _ -> assert false);
      let _, late =
        Openai.Responses_live.observe_legacy
          adapter
          ~entry_id:(Some host)
          (R.Response_stream.Output_item_done
             { item = Function_call provider_call
             ; output_index = 0
             ; type_ = "response.output_item.done"
             })
        |> ok
      in
      assert (List.is_empty late))
;;

let%test_unit "caller override also bounds exact no-op observations" =
  let started = event (Source_started { scope = root; origin = P.Origin.unavailable }) in
  let state = T.Draft.apply (draft ()) started |> ok |> fst in
  assert (Result.is_error (T.Draft.apply state ~max_retained_bytes:0 started));
  let state = T.Draft.mark_gap (draft ()) ~scope:(Some root.key) in
  let item = descriptor root "after-lost-scope" in
  let content = part item 0 Text in
  let state = apply state (Part_announced content) in
  let state =
    apply state (Changed { target = Content content; change = Append "suffix" })
  in
  match (text_of state).completeness with
  | Missing_prefix -> ()
  | Prefix_observed -> assert false
;;

let%test_unit
    "multipart order follows known numeric positions; calls retain exact input bytes"
  =
  let item = descriptor ~header:(Message Assistant) root "multipart" in
  let state =
    List.fold [ 10; 2; 0 ] ~init:(draft ()) ~f:(fun state index ->
      apply
        state
        (Changed
           { target = Content (part item index Text)
           ; change = Replace (Int.to_string index)
           }))
  in
  (match T.Draft.items state with
   | [ { state = Partial { parts; _ }; _ } ] ->
     assert (
       List.equal
         (Option.equal Int.equal)
         (List.map parts ~f:(fun part -> part.descriptor.index))
         [ Some 0; Some 2; Some 10 ])
   | _ -> assert false);
  let call = descriptor ~header:(Call Custom) ~call_name:"custom" root "call-input" in
  let bytes = " { \"not parsed\" : 1 } \n" in
  let state =
    apply (draft ()) (Changed { target = Call_input call; change = Append bytes })
  in
  match T.Draft.items state with
  | [ { state = Partial { call_input = Some text; _ }; _ } ] ->
    assert (String.equal text.value bytes);
    (match text.completeness with
     | Missing_prefix -> ()
     | Prefix_observed -> assert false)
  | _ -> assert false
;;

let%test_unit
    "provider item done remains provisional and opaque observations survive finalization"
  =
  let module R = Openai.Responses in
  let host = History_entry.Id.create ~namespace:"provider-final" ~sequence:0 |> ok in
  let message : R.Output_message.t =
    { role = Assistant
    ; id = "observed"
    ; content = [ { annotations = []; text = "done"; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
  in
  let adapter = Openai.Responses_live.create ~scope:root ~limits:document_limits in
  let adapter, observations =
    Openai.Responses_live.observe_legacy
      adapter
      ~entry_id:(Some host)
      (R.Response_stream.Output_item_done
         { item = Output_message message
         ; output_index = 3
         ; type_ = "response.output_item.done"
         })
    |> ok
  in
  assert (
    List.for_all observations ~f:(fun event ->
      match T.Stream.view event with
      | Item_finalized _ | Source_finished _ -> false
      | _ -> true));
  let committed =
    Openai.Responses_history.create_with_id_exn ~id:host (R.Item.Output_message message)
  in
  let adapter, final = Openai.Responses_live.finalized adapter committed |> ok in
  assert (
    List.count final ~f:(fun event ->
      match T.Stream.view event with
      | Item_finalized _ -> true
      | _ -> false)
    = 1);
  let raw =
    `Object
      [ "type", `String "future.observation"
      ; "item_id", `String "observed"
      ; "future", `Object [ "precise", `Number "1e+00"; "unicode", `String "完成" ]
      ]
  in
  let _, opaque =
    Openai.Responses_live.observe_legacy
      adapter
      ~entry_id:(Some host)
      (R.Response_stream.Unknown raw)
    |> ok
  in
  assert (
    List.exists opaque ~f:(fun event ->
      match T.Stream.view event with
      | Unknown_event { raw = actual; _ } -> Jsonaf.exactly_equal raw actual
      | _ -> false))
;;

let%test_unit "actual host correlation stabilizes changing legacy provider aliases" =
  let module R = Openai.Responses in
  let host = History_entry.Id.create ~namespace:"provider-alias" ~sequence:0 |> ok in
  let call : R.Function_call.t =
    { name = "inspect"
    ; arguments = ""
    ; call_id = "call-only-alias"
    ; _type = "function_call"
    ; id = None
    ; status = None
    }
  in
  let adapter = Openai.Responses_live.create ~scope:root ~limits:document_limits in
  let adapter, announced =
    Openai.Responses_live.observe_legacy
      adapter
      ~entry_id:(Some host)
      (R.Response_stream.Output_item_added
         { item = Function_call call
         ; output_index = 5
         ; type_ = "response.output_item.added"
         })
    |> ok
  in
  let _, delta =
    Openai.Responses_live.observe_legacy
      adapter
      ~entry_id:(Some host)
      (R.Response_stream.Function_call_arguments_delta
         { item_id = "later-provider-item"
         ; output_index = 5
         ; delta = "{ "
         ; type_ = "response.function_call_arguments.delta"
         })
    |> ok
  in
  let announced =
    List.find_map_exn announced ~f:(fun event ->
      match T.Stream.view event with
      | Item_announced item -> Some item
      | _ -> None)
  in
  let changed =
    List.find_map_exn delta ~f:(fun event ->
      match T.Stream.view event with
      | Changed { target = Call_input item; _ } -> Some item
      | _ -> None)
  in
  assert (T.Item.Key.equal (T.Item.key announced) (T.Item.key changed));
  assert (Option.equal History_entry.Id.equal changed.entry_id (Some host))
;;

let%test_unit "canonical depth survives presentation wrappers without weakening bytes" =
  let raw =
    List.init 125 ~f:Fn.id
    |> List.fold ~init:(`Number "1e+00") ~f:(fun nested _ -> `Array [ nested ])
  in
  let semantic =
    P.Semantic.create
      (Unknown { provider_kind = "future.deep" })
      ~metadata:P.Metadata.empty
    |> ok
  in
  let payload = P.captured semantic ~origin:P.Origin.unavailable ~raw |> ok in
  assert (Result.is_ok (P.validate payload));
  assert (
    Result.is_error
      (Document_schema.Json.validate
         ~limits:Document_schema.Limits.default
         (P.to_json payload)));
  let host = History_entry.Id.create ~namespace:"deep-payload" ~sequence:0 |> ok in
  let entry = History_entry.create_with_id ~id:host payload in
  let item =
    descriptor ~header:(T.Header.of_semantic semantic) ~entry_id:host root "deep"
  in
  let finalized = event (Item_finalized { item; entry }) in
  let json = T.Stream.to_json finalized in
  let bytes = T.Stream.encoded_bytes finalized in
  let limits max_bytes =
    T.Admission.limits ~max_bytes
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
    |> ok
  in
  let exact_limits = limits bytes in
  let admitted = T.Stream.of_json json ~limits:exact_limits |> ok in
  (match T.Stream.view admitted with
   | Item_finalized { entry = actual; _ } ->
     assert (History_entry.Id.equal host (History_entry.id actual));
     assert (
       Jsonaf.exactly_equal (P.to_json payload) (P.to_json (History_entry.payload actual)))
   | _ -> failwith "expected immutable finalized entry");
  let short_limits = limits (bytes - 1) in
  assert (Result.is_error (T.Stream.of_json json ~limits:short_limits));
  assert (Result.is_error (T.Admission.limits ~max_bytes:0))
;;

let%test_unit "presentation headroom cannot repair an invalid native authored payload" =
  let raw =
    List.init 125 ~f:Fn.id |> List.fold ~init:`Null ~f:(fun nested _ -> `Array [ nested ])
  in
  let semantic =
    P.Semantic.create
      (Message
         { form = Input
         ; role = Developer
         ; content = [ Unknown { kind = "future.deep"; raw } ]
         ; phase = Absent
         })
      ~metadata:P.Metadata.empty
    |> ok
  in
  let payload = P.authored semantic in
  assert (Result.is_error (P.validate payload));
  assert (
    Result.is_ok
      (Document_schema.Json.validate ~limits:document_limits (P.to_json payload)));
  let host = History_entry.Id.create ~namespace:"invalid-deep" ~sequence:0 |> ok in
  let entry = History_entry.create_with_id ~id:host payload in
  let item =
    descriptor ~header:(T.Header.of_semantic semantic) ~entry_id:host root "invalid"
  in
  assert (
    Result.is_error
      (T.Stream.create (Item_finalized { item; entry }) ~limits:document_limits))
;;
