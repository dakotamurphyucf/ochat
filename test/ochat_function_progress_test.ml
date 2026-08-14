open Core

module Echo = struct
  type input = string

  let name = "echo_progress"
  let type_ = "function"
  let description = Some "Echo progress test"
  let parameters = Jsonaf.of_string {|{"type":"string"}|}
  let input_of_string input = input
end

let output_text = function
  | Openai.Responses.Tool_output.Output.Text text -> text
  | Content _ -> failwith "expected text output"
;;

let show_progress { Ochat_function.Progress.channel; update } =
  let channel =
    match channel with
    | `Assistant -> "assistant"
    | `Reasoning -> "reasoning"
    | `Stdout -> "stdout"
    | `Stderr -> "stderr"
    | `Activity -> "activity"
  in
  let update =
    match update with
    | Append text -> "append:" ^ text
    | Replace text -> "replace:" ^ text
  in
  channel ^ ":" ^ update
;;

let tool =
  Ochat_function.create_streaming_function
    (module Echo)
    (fun ~invocation input ->
       Ochat_function.Invocation.emit
         invocation
         { channel = `Assistant; update = Append "first" };
       Ochat_function.Invocation.emit
         invocation
         { channel = `Activity; update = Replace "second" };
       Openai.Responses.Tool_output.Output.Text input)
;;

let%expect_test "direct run is silent and preserves final output" =
  print_endline (output_text (tool.run "final"));
  [%expect {| final |}]
;;

let%expect_test "progress runner emits ordered append and replace updates" =
  let progress = Queue.create () in
  let invocation =
    Ochat_function.Invocation.create (fun item -> Queue.enqueue progress item)
  in
  let output = tool.run_with_progress ~invocation "final" |> output_text in
  Queue.iter progress ~f:(fun item -> print_endline (show_progress item));
  print_endline output;
  [%expect
    {|
    assistant:append:first
    activity:replace:second
    final
    |}]
;;

let%expect_test "functions returns progress-aware runners" =
  let _, functions = Ochat_function.functions [ tool ] in
  let run = Hashtbl.find_exn functions Echo.name in
  let seen = ref false in
  let invocation = Ochat_function.Invocation.create (fun _ -> seen := true) in
  print_endline (output_text (run ~invocation "result"));
  print_s [%sexp (!seen : bool)];
  [%expect
    {|
    result
    true
    |}]
;;

let%expect_test "UTF-8 ingest retains boundaries and repairs malformed EOF" =
  let ingest = Chat_response.Utf8_ingest.create () in
  let bytes = "Aé€👍" in
  String.iter bytes ~f:(fun char ->
    Chat_response.Utf8_ingest.add ingest (String.of_char char) |> print_string);
  print_endline (Chat_response.Utf8_ingest.finish ingest);
  let malformed = Chat_response.Utf8_ingest.create () in
  print_string (Chat_response.Utf8_ingest.add malformed "\xF0\x9F");
  Chat_response.Utf8_ingest.finish malformed |> print_endline;
  [%expect
    {|
    Aé€👍
    �
    |}]
;;
