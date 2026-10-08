open Core
open Wire_tests

let%expect_test "known parts cannot be smuggled into the wrong content family" =
  List.iter
    [ {|{"type":"message","id":"m","status":"completed","role":"assistant","content":[{"type":"summary_text","text":"bad"}]}|}
    ; {|{"type":"reasoning","id":"r","summary":[{"type":"output_text","text":"bad","annotations":[]}]}|}
    ; {|{"type":"reasoning","id":"r","summary":[],"content":[{"type":"refusal","refusal":"bad"}]}|}
    ]
    ~f:(fun text ->
      printf "rejected:%b\n" (Result.is_error (W.Item.decode (json text) ~origin)));
  [%expect
    {|
    rejected:true
    rejected:true
    rejected:true
  |}]
;;

let%expect_test
    "added call identity and execution restrictions cannot change on finalization"
  =
  List.iter
    [ {|,"namespace":"restricted"|}
    ; {|,"async":true|}
    ; {|,"caller":{"type":"program","caller_id":"p"}|}
    ; ""
    ]
    ~f:(fun extra ->
      let added =
        json
          ("{\"type\":\"response.output_item.added\",\"sequence_number\":1,\"output_index\":0,\"item\":{\"id\":\"fc\",\"type\":\"function_call\",\"name\":\"lookup\",\"call_id\":\"call_1\",\"arguments\":\"\",\"status\":\"in_progress\""
           ^ extra
           ^ "}}")
      in
      let first = W.Tracker.add (W.Tracker.create origin) (event added) |> unwrap in
      let final =
        if String.is_empty extra
        then String.substr_replace_all call_json ~pattern:"lookup" ~with_:"other"
        else call_json
      in
      printf
        "rejected:%b\n"
        (Result.is_error (W.Tracker.add first.tracker (event (done_json 2 final)))));
  [%expect
    {|
    rejected:true
    rejected:true
    rejected:true
    rejected:true
  |}]
;;

let%expect_test "completed terminal cannot drop previously observed item slots" =
  let delta =
    json
      {|{"type":"response.function_call_arguments.delta","sequence_number":1,"output_index":0,"item_id":"fc","delta":"{}"}|}
  in
  let first = W.Tracker.add (W.Tracker.create origin) (event delta) |> unwrap in
  let completed =
    terminal_json "response.completed" 2 (base_response "completed" "[]" "")
  in
  printf "rejected:%b\n" (Result.is_error (W.Tracker.add first.tracker (event completed)));
  [%expect {| rejected:true |}]
;;

let%expect_test "final item cannot omit an already finalized content index" =
  let done_part =
    json
      {|{"type":"response.output_text.done","sequence_number":1,"output_index":0,"item_id":"m","content_index":1,"text":"lost","logprobs":[]}|}
  in
  let first = W.Tracker.add (W.Tracker.create origin) (event done_part) |> unwrap in
  let message =
    {|{"id":"m","type":"message","role":"assistant","status":"completed","content":[{"type":"output_text","text":"kept","annotations":[]}]}|}
  in
  printf
    "rejected:%b\n"
    (Result.is_error (W.Tracker.add first.tracker (event (done_json 2 message))));
  [%expect {| rejected:true |}]
;;

let%expect_test "incomplete or failed terminal-only calls are not newly executable" =
  List.iter
    [ "response.incomplete", "incomplete"; "response.failed", "failed" ]
    ~f:(fun (kind, status) ->
      let terminal =
        terminal_json kind 1 (base_response status ("[" ^ call_json ^ "]") "")
      in
      let transition =
        W.Tracker.add (W.Tracker.create origin) (event terminal) |> unwrap
      in
      printf
        "new:%d terminal:%b\n"
        (List.length transition.newly_finalized)
        (Result.is_ok (W.Tracker.finish transition.tracker)));
  [%expect
    {|
    new:0 terminal:true
    new:0 terminal:true
  |}]
;;

let%expect_test "origin mismatch and sequence reuse fail without mutating prior state" =
  let first =
    W.Tracker.add (W.Tracker.create origin) (event (done_json 1 call_json)) |> unwrap
  in
  let conflicting = event (json {|{"type":"response.future","sequence_number":1}|}) in
  printf "sequence:%b\n" (Result.is_error (W.Tracker.add first.tracker conflicting));
  let foreign_origin =
    W.Origin.create
      ~provider:"openai"
      ~account:(Some "account-b")
      ~endpoint:"public-responses"
    |> unwrap
  in
  let foreign = W.Event.decode (done_json 2 call_json) ~origin:foreign_origin |> unwrap in
  printf "origin:%b\n" (Result.is_error (W.Tracker.add first.tracker foreign));
  let repeated = W.Tracker.add first.tracker (event (done_json 1 call_json)) |> unwrap in
  printf "duplicate-new:%d\n" (List.length repeated.newly_finalized);
  [%expect
    {|
    sequence:true
    origin:true
    duplicate-new:0
  |}]
;;

let%expect_test
    "unknown event raw survives and malformed known event retains offending raw"
  =
  let stream = Codec.Stream.create origin |> Or_error.ok_exn in
  let raw = json {|{"type":"response.future","extra":[{"x":null}]}|} in
  ignore
    (Codec.Stream.feed_line stream ("data: " ^ Jsonaf.to_string raw) |> unwrap
     : Codec.Stream.update option);
  let update = Codec.Stream.feed_line stream "" |> unwrap |> Option.value_exn in
  printf "unknown-raw:%b\n" (Jsonaf.exactly_equal raw (W.Event.raw update.event));
  let malformed =
    json
      {|{"type":"response.output_item.done","sequence_number":2,"output_index":0,"item":{"type":"function_call","arguments":17}}|}
  in
  ignore
    (Codec.Stream.feed_line stream ("data: " ^ Jsonaf.to_string malformed) |> unwrap
     : Codec.Stream.update option);
  (match Codec.Stream.feed_line stream "" with
   | Error (Decode { raw; _ }) ->
     printf "malformed-raw:%b\n" (Jsonaf.exactly_equal raw malformed)
   | _ -> failwith "expected decode failure");
  [%expect
    {|
    unknown-raw:true
    malformed-raw:true
  |}]
;;

let%expect_test "error event retains its own terminal instead of becoming completed" =
  let terminal =
    event
      (json
         {|{"type":"error","sequence_number":1,"code":null,"param":null,"message":"provider failed"}|})
  in
  let transition = W.Tracker.add (W.Tracker.create origin) terminal |> unwrap in
  (match W.Tracker.finish transition.tracker with
   | Ok (Error error) -> printf "error:%s\n" error.message
   | _ -> failwith "wrong terminal");
  [%expect {| error:provider failed |}]
;;

let%expect_test "usage absent/null stays unknown; malformed negative usage rejects" =
  List.iter [ ""; ",\"usage\":null" ] ~f:(fun extra ->
    let response =
      W.Response.decode (base_response "completed" "[]" extra) ~origin |> unwrap
    in
    match W.Response.usage response with
    | Absent -> print_endline "absent"
    | Null -> print_endline "null"
    | Value _ -> failwith "invented usage");
  let raw =
    base_response
      "completed"
      "[]"
      {|,"usage":{"input_tokens":-1,"output_tokens":0,"total_tokens":0}|}
  in
  printf "negative:%b\n" (Result.is_error (W.Response.decode raw ~origin));
  [%expect
    {|
    absent
    null
    negative:true
  |}]
;;

let%expect_test
    "terminal echo ignores object key order but preserves original raw capture"
  =
  let first =
    W.Tracker.add (W.Tracker.create origin) (event (done_json 1 call_json)) |> unwrap
  in
  let reordered =
    {|{"arguments":"{}","status":"completed","call_id":"call_1","name":"lookup","type":"function_call","id":"fc"}|}
  in
  let terminal =
    terminal_json
      "response.completed"
      2
      (base_response "completed" ("[" ^ reordered ^ "]") "")
  in
  let transition = W.Tracker.add first.tracker (event terminal) |> unwrap in
  printf "new:%d\n" (List.length transition.newly_finalized);
  (match W.Tracker.finish transition.tracker with
   | Ok (Response { response; _ }) ->
     printf
       "terminal-raw:%b\n"
       (Jsonaf.exactly_equal
          (W.Item.raw (List.hd_exn (W.Response.output response)))
          (json reordered))
   | _ -> failwith "expected completed");
  [%expect
    {|
    new:0
    terminal-raw:true
  |}]
;;

let%expect_test
    "annotation references must reconcile with final content and annotation slots"
  =
  List.iter
    [ 1, 0, {|[{"type":"output_text","text":"x","annotations":[]}]|}
    ; ( 0
      , 1
      , {|[{"type":"output_text","text":"x","annotations":[{"type":"future","value":1}]}]|}
      )
    ; ( 0
      , 0
      , {|[{"type":"output_text","text":"x","annotations":[{"type":"future","value":2}]}]|}
      )
    ]
    ~f:(fun (content_index, annotation_index, content) ->
      let annotation =
        json
          (sprintf
             {|{"type":"response.output_text.annotation.added","sequence_number":1,"output_index":0,"item_id":"m","content_index":%d,"annotation_index":%d,"annotation":{"type":"future","value":1}}|}
             content_index
             annotation_index)
      in
      let first = W.Tracker.add (W.Tracker.create origin) (event annotation) |> unwrap in
      let message =
        sprintf
          {|{"id":"m","type":"message","role":"assistant","status":"completed","content":%s}|}
          content
      in
      printf
        "rejected:%b\n"
        (Result.is_error (W.Tracker.add first.tracker (event (done_json 2 message)))));
  [%expect
    {|
    rejected:true
    rejected:true
    rejected:true
  |}]
;;

let%expect_test "final snapshots may add optional restrictions without enabling execution"
  =
  let added =
    json
      {|{"type":"response.output_item.added","sequence_number":1,"output_index":0,"item":{"id":"fc","type":"function_call","name":"lookup","call_id":"call_1","arguments":"","status":"in_progress"}}|}
  in
  List.iter
    [ "namespace", `String "restricted"
    ; "async", `True
    ; "caller", `Object [ "type", `String "program"; "caller_id", `String "p" ]
    ]
    ~f:(fun (key, value) ->
      let first = W.Tracker.add (W.Tracker.create origin) (event added) |> unwrap in
      let final =
        match json call_json with
        | `Object fields -> `Object (fields @ [ key, value ])
        | _ -> assert false
      in
      let done_ =
        `Object
          [ "type", `String "response.output_item.done"
          ; "sequence_number", `Number "2"
          ; "output_index", `Number "0"
          ; "item", final
          ]
      in
      let transition = W.Tracker.add first.tracker (event done_) |> unwrap in
      printf "accepted-new:%d\n" (List.length transition.newly_finalized));
  [%expect
    {|
    accepted-new:0
    accepted-new:0
    accepted-new:0
  |}]
;;

let%expect_test
    "item conflict detail identifies unchanged strict rejection branches without private \
     values"
  =
  let module C = Inference.Observation.Diagnostic.Protocol_violation.Item_conflict in
  let raw id extras =
    `Object
      ([ "type", `String "reasoning"; "id", `String id; "summary", `Array [] ] @ extras)
  in
  let frame name sequence index item =
    W.Event.decode
      (`Object
          [ "type", `String name
          ; "sequence_number", `Number (Int.to_string sequence)
          ; "output_index", `Number (Int.to_string index)
          ; "item", item
          ])
      ~origin
    |> unwrap
  in
  let added item = frame "response.output_item.added" 0 0 item in
  let done_ item = frame "response.output_item.done" 1 0 item in
  let a = raw "PRIVATE_ID_A" [] in
  let phase value = raw "PRIVATE_ID_A" [ "phase", `String value ] in
  let encrypted value =
    raw
      "PRIVATE_ID_A"
      [ "encrypted_content", `String value; "PRIVATE_UNKNOWN_KEY", `String value ]
  in
  let cases =
    [ done_ a, frame "response.output_item.done" 2 0 (raw "PRIVATE_ID_B" [])
    ; done_ a, frame "response.output_item.done" 2 1 a
    ; ( added (phase "analysis")
      , frame "response.output_item.added" 2 0 (phase "final_answer") )
    ; done_ a, frame "response.output_item.added" 2 0 a
    ; ( added (phase "analysis")
      , frame "response.output_item.done" 2 0 (phase "final_answer") )
    ; ( done_ (encrypted "PRIVATE_EARLY")
      , frame "response.output_item.done" 2 0 (encrypted "PRIVATE_FINAL") )
    ]
  in
  List.iter cases ~f:(fun (first, failed) ->
    let original = (W.Tracker.add (W.Tracker.create origin) first |> unwrap).tracker in
    let index =
      match W.Tracker.add original failed with
      | Error (Item_conflict index) -> index
      | _ -> failwith "strict conflict required"
    in
    let detail =
      W.Tracker.item_conflict_detail original failed ~output_index:index
      |> Option.value_exn
    in
    let wire = C.to_json detail |> Jsonaf.to_string in
    assert (not (String.is_substring wire ~substring:"PRIVATE"));
    assert (C.equal detail (C.of_json (C.to_json detail) |> unwrap));
    print_endline wire);
  assert (
    Result.is_error
      (C.create ~event:Item_done ~cause:Final_snapshot_changed ~fields:[ Status; Status ]));
  assert (
    Result.is_error
      (C.create
         ~event:Item_done
         ~cause:Final_snapshot_changed
         ~fields:(List.init 17 ~f:(fun _ -> C.Other))));
  let original = (W.Tracker.add (W.Tracker.create origin) (done_ a) |> unwrap).tracker in
  let reordered =
    match a with
    | `Object fields -> `Object (List.rev fields)
    | _ -> assert false
  in
  assert (
    Result.is_ok
      (W.Tracker.add original (frame "response.output_item.done" 2 0 reordered)));
  assert (
    Option.is_none
      (W.Tracker.item_conflict_detail
         original
         (frame "response.output_item.done" 2 0 reordered)
         ~output_index:0));
  print_endline "duplicate bounded fields reject; reordered equal snapshot stays accepted";
  [%expect
    {|
{"event":"item_done","cause":"identity_changed","fields":["id"]}
{"event":"item_done","cause":"identity_reused","fields":["id"]}
{"event":"item_added","cause":"duplicate_added","fields":["phase"]}
{"event":"item_added","cause":"added_after_final","fields":[]}
{"event":"item_done","cause":"descriptor_changed","fields":["phase"]}
{"event":"item_done","cause":"final_snapshot_changed","fields":["encrypted_content","other"]}
duplicate bounded fields reject; reordered equal snapshot stays accepted
|}]
;;

let%expect_test "only terminal reasoning nonempty encryption representation may differ" =
  let reasoning ?(encrypted = Some (`String "EARLY_OPAQUE")) ?(extra = []) () =
    `Object
      ([ "type", `String "reasoning"; "id", `String "r"; "summary", `Array [] ]
       @ Option.to_list
           (Option.map encrypted ~f:(fun value -> "encrypted_content", value))
       @ extra)
  in
  let done_ item =
    event
      (`Object
          [ "type", `String "response.output_item.done"
          ; "sequence_number", `Number "1"
          ; "output_index", `Number "0"
          ; "item", item
          ])
  in
  let terminal item =
    event
      (`Object
          [ "type", `String "response.completed"
          ; "sequence_number", `Number "2"
          ; ( "response"
            , `Object
                [ "id", `String "response"
                ; "object", `String "response"
                ; "status", `String "completed"
                ; "output", `Array [ item ]
                ] )
          ])
  in
  let original_item = reasoning () in
  let original =
    (W.Tracker.add (W.Tracker.create origin) (done_ original_item) |> unwrap).tracker
  in
  let received = reasoning ~encrypted:(Some (`String "TERMINAL_OPAQUE")) () in
  let accepted = W.Tracker.add original (terminal received) |> unwrap in
  (match W.Tracker.finish accepted.tracker |> unwrap with
   | Response { response; _ } ->
     assert (
       Jsonaf.exactly_equal
         (W.Item.raw (List.hd_exn (W.Response.output response)))
         received)
   | Error _ -> assert false);
  assert (
    Option.is_none
      (W.Tracker.item_conflict_detail original (terminal received) ~output_index:0));
  let repeated =
    match done_ received |> W.Event.raw with
    | `Object fields ->
      event
        (`Object
            (List.map fields ~f:(fun (key, old) ->
               key, if String.equal key "sequence_number" then `Number "2" else old)))
    | _ -> assert false
  in
  assert (Result.is_error (W.Tracker.add original repeated));
  List.iter
    [ None; Some `Null; Some (`String "") ]
    ~f:(fun encrypted ->
      assert (
        Result.is_error (W.Tracker.add original (terminal (reasoning ~encrypted ())))));
  assert (
    Result.is_error
      (W.Tracker.add
         original
         (terminal
            (reasoning
               ~encrypted:(Some (`String "TERMINAL_OPAQUE"))
               ~extra:[ "future", `True ]
               ()))));
  let absent =
    (W.Tracker.add (W.Tracker.create origin) (done_ (reasoning ~encrypted:None ()))
     |> unwrap)
      .tracker
  in
  assert (Result.is_error (W.Tracker.add absent (terminal received)));
  let empty =
    (W.Tracker.add
       (W.Tracker.create origin)
       (done_ (reasoning ~encrypted:(Some (`String "")) ()))
     |> unwrap)
      .tracker
  in
  assert (Result.is_error (W.Tracker.add empty (terminal received)));
  assert (
    Result.is_error (W.Item.decode (reasoning ~encrypted:(Some (`Number "1")) ()) ~origin));
  let message =
    `Object
      [ "type", `String "message"
      ; "id", `String "m"
      ; "role", `String "assistant"
      ; "status", `String "completed"
      ; "content", `Array []
      ; "encrypted_content", `String "EARLY_OPAQUE"
      ]
  in
  let message_tracker =
    (W.Tracker.add (W.Tracker.create origin) (done_ message) |> unwrap).tracker
  in
  let changed_message =
    match message with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, old) ->
           ( name
           , if String.equal name "encrypted_content"
             then `String "TERMINAL_OPAQUE"
             else old )))
    | _ -> assert false
  in
  assert (Result.is_error (W.Tracker.add message_tracker (terminal changed_message)));
  print_endline
    "raw terminal retained; repeated done, presence/type/empty/unknown/nonreasoning \
     changes reject";
  [%expect
    {| raw terminal retained; repeated done, presence/type/empty/unknown/nonreasoning changes reject |}]
;;
