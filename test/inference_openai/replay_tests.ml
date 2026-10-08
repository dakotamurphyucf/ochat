open! Core
module Replay = Openai.Responses_replay
module Origin = History_entry.Payload.Origin

let ok result = Result.map_error result ~f:(fun _ -> "fixture") |> Result.ok_or_failwith

let origin
      ?(account = "account")
      ?(endpoint = "https://api.openai.com/v1/responses")
      model
  =
  Origin.create
    ~adapter:"openai.responses"
    ~provider:"selected"
    ~account:(Some account)
    ~endpoint
    ~profile:(Some "selected")
    ~model:(Some model)
    ~replay_version:1
  |> ok
;;

let json = Jsonaf.of_string

let message =
  json
    {|{"type":"message","id":"m","status":"completed","role":"assistant","phase":"commentary","content":[{"type":"output_text","text":"text","annotations":[]}]}|}
;;

let function_call =
  json
    {|{"type":"function_call","id":"f","status":"completed","call_id":"c","name":"inspect","arguments":" { exact } "}|}
;;

let custom_call =
  json
    {|{"type":"custom_tool_call","id":"x","call_id":"x","name":"shell","input":"echo preserved"}|}
;;

let reasoning =
  json
    {|{"type":"reasoning","id":"r","summary":[{"type":"summary_text","text":"summary"}],"encrypted_content":"opaque"}|}
;;

let extension raw =
  match raw with
  | `Object fields -> `Object (fields @ [ "future", `Null ])
  | _ -> assert false
;;

let%expect_test
    "directed closed replay policy preserves origin and refuses unknown shapes"
  =
  let policy =
    Replay.create
      ~transitions:
        [ "a", "b", [ Assistant_text; Function_call; Custom_call ]
        ; "b", "c", [ Assistant_text ]
        ]
    |> ok
  in
  let permits ?(from = "a") ?(to_ = "b") raw =
    Replay.permits policy ~actual:(origin from) ~expected:(origin to_) ~raw
  in
  List.iter [ message; function_call; custom_call ] ~f:(fun raw ->
    assert (permits raw);
    assert (not (permits (extension raw)));
    assert (permits ~to_:"a" (extension raw)));
  assert (not (permits reasoning));
  assert (not (permits ~from:"b" ~to_:"a" message));
  assert (not (permits ~to_:"c" message));
  assert (
    not
      (Replay.permits
         policy
         ~actual:(origin "a")
         ~expected:(origin ~account:"other" "b")
         ~raw:message));
  assert (
    not
      (Replay.permits
         policy
         ~actual:Origin.unavailable
         ~expected:Origin.unavailable
         ~raw:message));
  List.iter
    [ {|{"type":"message","role":"assistant","content":[{"type":"output_text","text":"x","annotations":[{"future":"value"}]}]}|}
    ; {|{"type":"message","role":"assistant","phase":"future","content":[]}|}
    ; {|{"type":"function_call","name":"f","arguments":"{}","call_id":"c","namespace":"tools"}|}
    ; {|{"type":"function_call","name":"f","arguments":"{}","call_id":"c","async":true}|}
    ; {|{"type":"compaction","encrypted_content":"opaque"}|}
    ]
    ~f:(fun text -> assert (not (permits (json text))));
  let qualified = Replay.create ~transitions:[ "a", "b", [ Reasoning ] ] |> ok in
  assert (
    Replay.permits qualified ~actual:(origin "a") ~expected:(origin "b") ~raw:reasoning);
  assert (
    not
      (Replay.permits
         qualified
         ~actual:(origin "a")
         ~expected:(origin "b")
         ~raw:(extension reasoning)));
  assert (
    Result.is_error
      (Replay.create
         ~transitions:[ "a", "b", [ Assistant_text ]; "a", "b", [ Reasoning ] ]));
  assert (
    Result.is_error
      (Replay.create ~transitions:[ "a", "b", [ Assistant_text; Assistant_text ] ]));
  print_endline
    "known classes admitted; raw extensions, reverse/transitive pairs, other accounts \
     and unavailable origins refused";
  [%expect
    {| known classes admitted; raw extensions, reverse/transitive pairs, other accounts and unavailable origins refused |}]
;;
