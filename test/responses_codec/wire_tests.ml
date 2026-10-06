open Core
module Codec = Openai.Responses.Codec
module W = Codec.Wire

let unwrap = function
  | Ok value -> value
  | Error _ -> failwith "unexpected codec failure"
;;

let json = Jsonaf.of_string

let origin =
  W.Origin.create
    ~provider:"openai"
    ~account:(Some "account-a")
    ~endpoint:"public-responses"
  |> unwrap
;;

let item text = W.Item.decode (json text) ~origin |> unwrap
let response text = W.Response.decode (json text) ~origin |> unwrap

let%expect_test "ordered content, refusal, phase and future extras remain recoverable" =
  let raw =
    json
      {|{"type":"message","role":"assistant","id":"msg_1","status":"completed","phase":"final_answer","future":{"n":null},"content":[{"type":"output_text","text":"before","annotations":[],"new_field":17},{"type":"refusal","refusal":"cannot"},{"type":"future_part","payload":[1,2]},{"type":"output_text","text":"after","annotations":[]}]}|}
  in
  let captured = W.Item.decode raw ~origin |> unwrap in
  (match W.Item.view captured with
   | Message { content; phase } ->
     print_s [%sexp (phase : W.Phase.t W.Presence.t)];
     List.iter content ~f:(fun part ->
       match W.Part.view part with
       | Output_text { text; _ } -> printf "text:%s\n" text
       | Refusal text -> printf "refusal:%s\n" text
       | Unknown name -> printf "unknown:%s\n" name
       | Summary_text _ | Reasoning_text _ -> failwith "wrong part")
   | _ -> failwith "wrong item");
  printf "exact-raw:%b\n" (Jsonaf.exactly_equal raw (W.Item.raw captured));
  printf "origin:%b\n" (W.Origin.equal origin (W.Item.origin captured));
  [%expect
    {|
(Value Final_answer)
text:before
refusal:cannot
unknown:future_part
text:after
exact-raw:true
origin:true
|}]
;;

let%expect_test "opaque reasoning preserves absent, null, value and unknown data" =
  List.iter
    [ {|{"type":"reasoning","id":"r","summary":[]}|}
    ; {|{"type":"reasoning","id":"r","summary":[],"encrypted_content":null}|}
    ; {|{"type":"reasoning","id":"r","summary":[],"encrypted_content":"opaque==\nbytes","content":[{"type":"reasoning_text","text":"provided summary"}],"future":[null]}|}
    ]
    ~f:(fun text ->
      let captured = item text in
      (match W.Item.view captured with
       | Reasoning { encrypted_content; _ } ->
         (match encrypted_content with
          | Absent -> print_endline "Absent"
          | Null -> print_endline "Null"
          | Value text -> print_endline (Jsonaf.to_string (`String text)))
       | _ -> failwith "wrong item");
      printf
        "raw:%b executable:%b\n"
        (Jsonaf.exactly_equal (json text) (W.Item.raw captured))
        (Option.is_some (W.Item.local_call captured)));
  [%expect
    {|
Absent
raw:true executable:false
Null
raw:true executable:false
"opaque==\nbytes"
raw:true executable:false
|}]
;;

let%expect_test "tool payloads remain exact strings, never parsed or normalized" =
  let function_call =
    item
      {|{"type":"function_call","id":"fc","name":"lookup","call_id":"c","arguments":" { \"x\" : 1e0 }\n","status":"completed","future":true}|}
  in
  let custom_call =
    item
      {|{"type":"custom_tool_call","id":"ct","name":"edit","call_id":"d","input":"*** Begin Patch\n+λ\n*** End Patch"}|}
  in
  List.iter [ function_call; custom_call ] ~f:(fun captured ->
    match W.Item.local_call captured with
    | Some (Function { arguments; _ }) ->
      printf "function-exact:%b\n" (String.equal arguments " { \"x\" : 1e0 }\n")
    | Some (Custom { input; _ }) ->
      printf "custom-exact:%b\n" (String.equal input "*** Begin Patch\n+λ\n*** End Patch")
    | None -> failwith "expected selected local call");
  [%expect
    {|
function-exact:true
custom-exact:true
|}]
;;

let%expect_test "unknown and unsupported call semantics never become local calls" =
  List.iter
    [ {|{"type":"future_call","name":"shell","arguments":"danger","call_id":"c"}|}
    ; {|{"type":"function_call","name":"shell","call_id":"c","arguments":"{}","namespace":"remote"}|}
    ; {|{"type":"function_call","name":"shell","call_id":"c","arguments":"{}","async":true}|}
    ; {|{"type":"function_call","name":"shell","call_id":"c","arguments":"{}","status":"in_progress"}|}
    ]
    ~f:(fun text -> printf "%b\n" (Option.is_some (W.Item.local_call (item text))));
  [%expect
    {|
false
false
false
false
|}]
;;

let%expect_test "malformed selected kinds and duplicate keys reject" =
  List.iter
    [ {|{"type":"function_call","name":"x","call_id":"c","arguments":{}}|}
    ; {|{"type":"custom_tool_call","name":"x","call_id":"c"}|}
    ; {|{"type":"message","id":"m","role":"assistant","status":"completed","content":[{"type":"refusal","text":"wrong key"}]}|}
    ; {|{"type":"reasoning","id":"r","summary":[],"encrypted_content":12}|}
    ; {|{"type":"future","type":"function_call","arguments":"{}"}|}
    ]
    ~f:(fun text ->
      printf "rejected:%b\n" (Result.is_error (W.Item.decode (json text) ~origin)));
  [%expect
    {|
rejected:true
rejected:true
rejected:true
rejected:true
rejected:true
|}]
;;

let base_response status output extra =
  json
    ("{\"id\":\"resp_1\",\"object\":\"response\",\"created_at\":1,\"model\":\"gpt-test\",\"status\":\""
     ^ status
     ^ "\",\"output\":"
     ^ output
     ^ ",\"parallel_tool_calls\":true,\"tools\":[],\"tool_choice\":\"auto\""
     ^ extra
     ^ "}")
;;

let outcome = function
  | W.Response.Completed -> "completed"
  | Refused -> "refused"
  | Incomplete _ -> "incomplete"
  | Failed _ -> "failed"
  | Nonterminal _ -> "nonterminal"
;;

let%expect_test "semantic outcome is separate from refusal content and item completion" =
  List.iter
    [ "completed", "[]", ""
    ; ( "completed"
      , {|[{"id":"m","type":"message","role":"assistant","status":"completed","content":[{"type":"refusal","refusal":"cannot"}]}]|}
      , "" )
    ; ( "incomplete"
      , "[]"
      , {|,"incomplete_details":{"reason":"max_output_tokens","future":3}|} )
    ; "failed", "[]", {|,"error":{"code":"server_error","message":"failed"}|}
    ; "in_progress", "[]", ""
    ]
    ~f:(fun (status, output, extra) ->
      let captured =
        W.Response.decode (base_response status output extra) ~origin |> unwrap
      in
      print_endline (outcome (W.Response.outcome captured)));
  [%expect
    {|
completed
refused
incomplete
failed
nonterminal
|}]
;;

let%expect_test "normalized usage preserves detail counts without adding them twice" =
  let raw =
    base_response
      "completed"
      "[]"
      {|,"usage":{"input_tokens":100,"output_tokens":25,"total_tokens":125,"input_tokens_details":{"cached_tokens":80,"cache_write_tokens":4,"future":1},"output_tokens_details":{"reasoning_tokens":20},"unknown":null}|}
  in
  let captured = W.Response.decode raw ~origin |> unwrap in
  (match W.Response.usage captured with
   | Value usage ->
     printf
       "%Ld %Ld %Ld\n"
       (W.Usage.input_tokens usage)
       (W.Usage.output_tokens usage)
       (W.Usage.total_tokens usage);
     print_s [%sexp (W.Usage.cached_tokens usage : int64 W.Presence.t)];
     print_s [%sexp (W.Usage.reasoning_tokens usage : int64 W.Presence.t)]
   | Absent | Null -> failwith "missing usage");
  printf "retained:%b\n" (Jsonaf.exactly_equal raw (W.Response.raw captured));
  [%expect
    {|
100 25 125
(Value 80)
(Value 20)
retained:true
|}]
;;

let call_json =
  {|{"id":"fc","type":"function_call","name":"lookup","call_id":"call_1","arguments":"{}","status":"completed"}|}
;;

let done_json sequence call =
  json
    (sprintf
       {|{"type":"response.output_item.done","sequence_number":%d,"output_index":0,"item":%s}|}
       sequence
       call)
;;

let terminal_json kind sequence response =
  `Object
    [ "type", `String kind
    ; "sequence_number", `Number (Int.to_string sequence)
    ; "response", response
    ]
;;

let event raw = W.Event.decode raw ~origin |> unwrap

let%expect_test
    "only one finalized call is published across duplicate done and terminal echo"
  =
  let tracker = W.Tracker.create origin in
  let one = W.Tracker.add tracker (event (done_json 1 call_json)) |> unwrap in
  let two = W.Tracker.add one.tracker (event (done_json 2 call_json)) |> unwrap in
  let three =
    W.Tracker.add
      two.tracker
      (event
         (terminal_json
            "response.completed"
            3
            (base_response "completed" ("[" ^ call_json ^ "]") "")))
    |> unwrap
  in
  List.iter [ one; two; three ] ~f:(fun transition ->
    printf "new:%d\n" (List.length transition.W.Tracker.newly_finalized));
  printf "complete:%b\n" (Result.is_ok (W.Tracker.finish three.tracker));
  let conflicting = String.substr_replace_all call_json ~pattern:"{}" ~with_:"{ }" in
  printf
    "conflict:%b\n"
    (Result.is_error (W.Tracker.add one.tracker (event (done_json 2 conflicting))));
  [%expect
    {|
new:1
new:0
new:0
complete:true
conflict:true
|}]
;;

let%expect_test
    "SSE facade retains unknown events and distinguishes failed, incomplete, EOF"
  =
  let run lines =
    let stream = Codec.Stream.create origin |> Or_error.ok_exn in
    List.iter lines ~f:(fun line ->
      ignore (Codec.Stream.feed_line stream line |> unwrap : Codec.Stream.update option));
    match Codec.Stream.finish stream with
    | Ok (W.Tracker.Response { terminal; _ }) ->
      print_s [%sexp (terminal : W.Event.terminal)]
    | Ok (W.Tracker.Error _) -> print_endline "provider-error"
    | Error (Protocol { error = Truncated; _ }) -> print_endline "truncated"
    | Error _ -> print_endline "other-error"
  in
  let frame raw = [ "data: " ^ Jsonaf.to_string raw; "" ] in
  List.iter
    [ "response.completed", "completed"
    ; "response.incomplete", "incomplete"
    ; "response.failed", "failed"
    ]
    ~f:(fun (kind, status) ->
      run (frame (terminal_json kind 1 (base_response status "[]" ""))));
  run
    (frame
       (json {|{"type":"response.future","sequence_number":1,"extra":{"kept":true}}|}));
  run (frame (done_json 1 call_json));
  [%expect
    {|
Completed
Incomplete
Failed
truncated
truncated
|}]
;;

let%expect_test "DONE marker without semantic terminal fails and preserves failure" =
  let stream = Codec.Stream.create origin |> Or_error.ok_exn in
  ignore
    (Codec.Stream.feed_line stream "data: [DONE]" |> unwrap : Codec.Stream.update option);
  printf "marker-error:%b\n" (Result.is_error (Codec.Stream.feed_line stream ""));
  printf "finish-error:%b\n" (Result.is_error (Codec.Stream.finish stream));
  [%expect
    {|
marker-error:true
finish-error:true
|}]
;;
