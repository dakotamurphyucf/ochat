open! Core
module Output = Openai.Responses.Tool_output.Output

module Echo : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "echo"
  let type_ = "function"
  let description = Some "Return the supplied text"

  let parameters =
    Jsonaf.of_string
      {|{"type":"object","properties":{"text":{"type":"string"}},"required":["text"],"additionalProperties":false}|}
  ;;

  let input_of_string input =
    Jsonaf.of_string input |> Jsonaf.member_exn "text" |> Jsonaf.string_exn
  ;;
end

let echo = Ochat_function.create_function (module Echo) (fun text -> Output.Text text)

let observed_echo =
  Ochat_function.create_streaming_function
    (module Echo)
    (fun ~invocation text ->
       Ochat_function.Invocation.emit
         invocation
         { channel = `Activity; update = Replace "Preparing response" };
       Output.Text text)
;;

let require predicate message = if not predicate then failwith message

let is_text expected = function
  | Output.Text actual -> String.equal expected actual
  | Content _ -> false
;;

let check_dispatch () =
  let metadata, dispatch = Ochat_function.functions [ echo ] in
  require (List.length metadata = 1) "missing metadata";
  let run = Hashtbl.find_exn dispatch "echo" in
  let output = run ~invocation:Ochat_function.Invocation.silent {|{"text":"Hello"}|} in
  require (is_text "Hello" output) "incorrect dispatch output";
  require
    (Result.is_error (Result.try_with (fun () -> echo.run {|{"missing":true}|})))
    "malformed arguments were accepted";
  require
    (Result.is_error
       (Result.try_with (fun () -> Ochat_function.functions [ echo; echo ])))
    "duplicate tool names were accepted"
;;

let check_progress () =
  let updates = ref [] in
  let invocation =
    Ochat_function.Invocation.create (fun update -> updates := update :: !updates)
  in
  let output = observed_echo.run_with_progress ~invocation {|{"text":"Hello"}|} in
  require (is_text "Hello" output) "progress replaced the final result";
  require
    (match !updates with
     | [ { channel = `Activity; update = Replace "Preparing response" } ] -> true
     | _ -> false)
    "incorrect progress";
  require
    (is_text "Hello" (observed_echo.run {|{"text":"Hello"}|}))
    "silent output changed";
  require (List.length !updates = 1) "silent dispatch emitted observed progress"
;;

let check_content () =
  let tool =
    Ochat_function.create_function
      (module Echo)
      (fun text -> Output.Content [ Input_text { text } ])
  in
  require
    (match tool.run {|{"text":"Hello"}|} with
     | Output.Content [ Input_text { text = "Hello"; _ } ] -> true
     | _ -> false)
    "structured output was lost"
;;

let check_trace () =
  let traces = ref [] in
  let invocation =
    Ochat_function.Invocation.create_with_trace ~progress:ignore ~trace:(fun trace ->
      traces := trace :: !traces)
  in
  Ochat_function.Invocation.emit_trace
    invocation
    (Tool_started { call_id = "child"; name = "echo"; kind = `Function; payload = "{}" });
  Ochat_function.Invocation.emit_trace
    invocation
    (Tool_finished
       { call_id = "child"; outcome = Returned; output = Some (Output.Text "ok") });
  require (List.length !traces = 2) "nested trace was lost";
  require (Ochat_function.Invocation.is_observed invocation) "observer was not detected";
  require
    (not (Ochat_function.Invocation.is_observed Ochat_function.Invocation.silent))
    "silent invocation reported an observer"
;;

let () =
  check_dispatch ();
  check_progress ();
  check_content ();
  check_trace ();
  print_endline "Custom-tool documentation example passed (offline)"
;;
