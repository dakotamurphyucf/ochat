open! Core
module P = Agent_protocol

let code result =
  match result with
  | Ok _ -> "ok"
  | Error error -> P.Error.code_to_string error.P.Error.code
;;

let%expect_test "content revision rejects negatives and never wraps" =
  let module R = P.History.Content_revision in
  print_endline (code (R.of_int64 (-1L)));
  print_endline (code (R.of_json (`Number "-1")));
  print_endline (code (R.of_json (`Number "9223372036854775808")));
  let maximum =
    R.of_int64 Int64.max_value
    |> function
    | Ok value -> value
    | Error _ -> failwith "fixture admission failed"
  in
  print_endline (code (R.succ maximum));
  let next =
    R.succ R.zero
    |> function
    | Ok value -> value
    | Error _ -> failwith "fixture admission failed"
  in
  print_endline (Int64.to_string (R.to_int64 next));
  [%expect
    {|invalid_request
invalid_request
invalid_request
invalid_request
1|}]
;;

let%expect_test "edit intent validates complete UTF-8 text at decode and construction" =
  let id =
    History_entry.Id.create ~namespace:"fixture-history" ~sequence:0
    |> function
    | Ok value -> value
    | Error _ -> failwith "fixture admission failed"
  in
  let create text =
    P.History_edit.create
      ~history_id:id
      ~expected_content_revision:P.History.Content_revision.zero
      ~text
      ~mode:P.History_edit.Mode.Save_only
  in
  print_endline (code (create "\255"));
  print_endline (code (create (String.make 1_048_577 'a')));
  let intent =
    create ""
    |> function
    | Ok value -> value
    | Error _ -> failwith "fixture admission failed"
  in
  let restored =
    P.History_edit.of_json (P.History_edit.to_json intent)
    |> function
    | Ok value -> value
    | Error _ -> failwith "fixture admission failed"
  in
  printf
    "empty=%b same-id=%b\n"
    (String.is_empty (P.History_edit.text restored))
    (P.History.Id.equal id (P.History_edit.history_id restored));
  let json = P.History_edit.to_json intent in
  let fields =
    match json with
    | `Object fields -> fields
    | _ -> assert false
  in
  print_endline
    (code (P.History_edit.of_json (`Object (("mode", `String "save_only") :: fields))));
  [%expect
    {|invalid_request
invalid_request
empty=true same-id=true
invalid_request|}]
;;

let%expect_test "pending input conflict is an actionable shared protocol code" =
  let original =
    P.Error.create
      Pending_input_conflict
      ~message:"select pending input explicitly"
      ~retryable:false
      ()
  in
  let restored =
    match P.Error.of_json (P.Error.to_json original) with
    | Ok restored -> restored
    | Error _ -> failwith "pending input code fixture"
  in
  printf
    "%s preserved=%b\n"
    (P.Error.code_to_string restored.code)
    (P.Error.equal_code original.code restored.code);
  [%expect {|pending_input_conflict preserved=true|}]
;;
