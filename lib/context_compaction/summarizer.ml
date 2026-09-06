open! Core

(*------------------------------------------------------------------*)
(*  Internal helpers                                                 *)
(*------------------------------------------------------------------*)

let prompt =
  {|
You will be given a conversation history of an llm conversation and your task is to compact it to save on context space by creating a concise summary that captures the user's intent and key technical details, paying close attention to the user's explicit requests and your previous actions.
This summary should be thorough in capturing technical details, code patterns, architectural decisions, assistant outputs, and any other context that would be essential for continuing development work without losing context.

Before providing your final summary, wrap your analysis of the conversation in <analysis> tags to have a rough outline of the main themes of the conversation and an analysis of each section.

Your Detailed summary should include the following sections:

1. Primary Request and Intent: Capture all of the user's explicit requests and intents in detail
2. Key Technical Concepts: List all important technical concepts, technologies, and frameworks discussed.
3. Files and Code Sections: Enumerate specific files and code sections examined, modified, or created. Pay special attention to the most recent messages and include full code snippets where applicable and include a summary of why this file read or edit is important.
4. Errors and fixes: List all errors that you ran into, and how you fixed them. Pay special attention to specific user feedback that you received, especially if the user told you to do something differently.
5. Problem Solving: Document problems solved and any ongoing troubleshooting efforts.
6. Abride version of All relevant user messages: List ALL user messages. These are critical for understanding the users' feedback and changing intent.
7. Abride valid_float_lexemAll relevant assistant messages: List ALL assistant messages. These are critical for understanding the assistant's responses and actions. Please ensure you are thorough and complete. If you outputted anything significant, it should be included here in detail.
8. Pending Tasks: Outline any pending tasks that you have explicitly been asked to work on.
9. Current Work: Describe in detail precisely what was being worked on immediately before this summary request, paying special attention to the most recent messages from both user and assistant. Include file names and code snippets where applicable.
10. Optional Next Step: List the next step that you will take that is related to the most recent work you were doing. IMPORTANT: ensure that this step is DIRECTLY in line with the task you were working on immediately before this summary request. If your last task was concluded, then only list next steps if they are explicitly in line with the current context such as todo list.
    if there is a next step, include direct quotes from the most recent conversation showing exactly what task you were working on and where you left off. This should be verbatim to ensure there's no drift in task interpretation.
    Include a list of commands that should be run to continue the work, and any other relevant information that would be needed to continue the work and get up to speed quickly.

Here's an example of how your output should be structured:

<example>
<analysis>
[Chronological analysis of the conversation, ensuring all points are covered thoroughly and accurately. You must be detailed and precise in your analysis]
</analysis>

<summary>
1. Primary Request and Intent:
   [Detailed description]

2. Key Technical Concepts:
   - [Concept 1]
   - [Concept 2]
   - [...]

3. Files and Code Sections:
   - [File Name 1]
      - [Summary of why this file is important]
      - [Summary of the changes made to this file, if any]
      - [Important Code Snippet]
   - [File Name 2]
      - [Important Code Snippet]
   - [...]

4. Errors and fixes:
    - [Detailed description of error 1]:
      - [How you fixed the error]
      - [User feedback on the error if any]
    - [...]

5. Problem Solving:
   [Detailed Description of solved problems and ongoing troubleshooting]
   - [Description of problem 1]
      - [Important outputs or results]
   - [Description of problem 2]
     - [Important outputs or results]
   - [...]

6. All user messages:
    - [Detailed user message]
    - [...]

7. All relevant assistant messages:
    - [Detailed assistant message]
    - [...]

8. Pending Tasks:
   - [Task 1]
   - [Task 2]
   - [...]

9. Current Work:
   [Precise description of current work]

10. Optional Next Step:
   [Optional Next step to take]

</summary>

<important-details>
[Important details / snippets that should be included verbatim]
</important-details>
</example>

Please provide your Detailed summary based on the conversation so far, following this structure and ensuring precision and thoroughness in your response.
- You must ensure that your summary contains any all the relevant information needed to pick up where you left off in the most condensed manner possible.
- You will not be able to remember any information from this conversation after this summary is provided, so ensure that you capture all relevant information.
- If you do not save relevant information in this summary, you will not be able to continue the conversation effectively and that is not acceptable.
- You must assume that the user will not remember any details from this conversation, so you must ensure that your summary optimally is condensed but still complete.
- Think about information that the user or agent might want to reference in the future, and ensure that it is included in your summary.
- you should be going for 90% compression of the conversation, so you must be thorough and complete in your analysis but be consise in your phrasing.
- provide a section <important-details></important-details> whose contents should include any critical details or snippets that should be included verbatim.
  Use your own judgement to determine what is critical and what can be omitted based on the conversation so far. An example would be a final draft of a proposal or report that was not saved and is relevant to the current conversation.
- do not duplicate information in analysis and summary sections.

conversation with inputs of the kind:
```
<user>
<system-reminder>This is a message from the system that we compacted the conversation history from a previous session.
Here is a summary of the session that you saved:
<analysis></analysis>
<summary></summary>
<important-details></important-details>
</system-reminder>
</user>
```
are compactions from previous conversations with the user.


|}
;;

let max_stub_chars = 2_000

exception Missing_summary
exception Retries_exhausted of exn

let string_of_input_content (content : Openai.Responses.Input_message.content_item list)
  : string
  =
  let open Openai.Responses.Input_message in
  content
  |> List.map ~f:(function
    | Text { text; _ } -> text
    | Image { image_url; _ } -> Printf.sprintf "<image src=\"%s\" />" image_url)
  |> String.concat ~sep:"\n"
;;

let string_of_output_content (content : Openai.Responses.Output_message.content list)
  : string
  =
  content |> List.map ~f:(fun part -> part.text) |> String.concat ~sep:"\n"
;;

let render_item (item : Openai.Responses.Item.t) : string option =
  let open Openai.Responses in
  let string_of_tool_output (output : Tool_output.Output.t) : string =
    match output with
    | Tool_output.Output.Text text -> text
    | Content parts ->
      parts
      |> List.map ~f:(function
        | Tool_output.Output_part.Input_text { text } -> text
        | Input_image { image_url; _ } -> Printf.sprintf "<image src=\"%s\" />" image_url)
      |> String.concat ~sep:"\n"
  in
  match item with
  | Item.Input_message { content; role; _ } ->
    if List.is_empty content
    then None
    else
      sprintf
        "%s: %s"
        (Input_message.role_to_string role)
        (string_of_input_content content)
      |> Some
  | Item.Output_message { content; _ } ->
    if List.is_empty content
    then None
    else sprintf "Assistant: %s" (string_of_output_content content) |> Some
  | Function_call { name; arguments; call_id; _ } ->
    sprintf "Function call (%s): %s(%s)" call_id name arguments |> Some
  | Custom_tool_call { name; input; call_id; _ } ->
    sprintf "Custom tool call (%s): %s(%s)" call_id name input |> Some
  | Function_call_output { call_id; output; _ } ->
    let output = string_of_tool_output output in
    sprintf "Function call output (%s): %s" call_id output |> Some
  | Custom_tool_call_output { call_id; output; _ } ->
    let output = string_of_tool_output output in
    sprintf "Custom tool call output (%s): %s" call_id output |> Some
  | _ -> None
;;

let render_transcript (items : Openai.Responses.Item.t list) : string =
  items |> List.filter_map ~f:render_item |> String.concat ~sep:"\n"
;;

let retry_request ~sleep ~request =
  let rec loop attempt =
    match request () with
    | summary -> summary
    | (exception Openai.Responses.Response_stream_parsing_error (_, cause))
    | (exception Openai.Responses.Response_parsing_error (_, cause)) ->
      if attempt >= 3
      then raise (Retries_exhausted cause)
      else (
        sleep (Float.of_int attempt);
        loop (attempt + 1))
  in
  loop 1
;;

let is_previous_compaction (item : Openai.Responses.Item.t) =
  let open Openai.Responses in
  match item with
  | Item.Input_message { role = User; content = Text { text; _ } :: _; _ } ->
    String.strip text |> String.is_prefix ~prefix:"<system-reminder>"
  | _ -> false
;;

let is_shared_context (item : Openai.Responses.Item.t) =
  let open Openai.Responses in
  match item with
  | Item.Input_message { role = System | Developer; _ } -> true
  | _ -> is_previous_compaction item
;;

let call_key (item : Openai.Responses.Item.t) =
  let open Openai.Responses in
  match item with
  | Item.Function_call { call_id; _ } -> Some ("function:" ^ call_id)
  | Item.Custom_tool_call { call_id; _ } -> Some ("custom:" ^ call_id)
  | _ -> None
;;

let output_key (item : Openai.Responses.Item.t) =
  let open Openai.Responses in
  match item with
  | Item.Function_call_output { call_id; _ } -> Some ("function:" ^ call_id)
  | Item.Custom_tool_call_output { call_id; _ } -> Some ("custom:" ^ call_id)
  | _ -> None
;;

let grouped_items items =
  let update_pending pending item =
    let pending =
      match call_key item with
      | None -> pending
      | Some key -> Set.add pending key
    in
    match output_key item with
    | None -> pending
    | Some key -> Set.remove pending key
  in
  let rec consume_calls pending acc = function
    | [] -> List.rev acc, []
    | item :: rest ->
      let pending = update_pending pending item in
      let acc = item :: acc in
      if Set.is_empty pending then List.rev acc, rest else consume_calls pending acc rest
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | item :: rest ->
      (match call_key item with
       | None -> loop ([ item ] :: acc) rest
       | Some key ->
         let group, rest =
           consume_calls (Set.singleton (module String) key) [ item ] rest
         in
         loop (group :: acc) rest)
  in
  loop [] items
;;

let rolling_summary_item text =
  let open Openai.Responses in
  Item.Input_message
    { role = User
    ; content =
        [ Input_message.Text
            { text =
                Printf.sprintf
                  "<previous-compaction-result>\n%s\n</previous-compaction-result>"
                  text
            ; _type = "input_text"
            }
        ]
    ; _type = "message"
    }
;;

let label_results results =
  results
  |> List.mapi ~f:(fun index result ->
    Printf.sprintf
      "<compaction-part index=\"%d\">\n%s\n</compaction-part>"
      (index + 1)
      result)
  |> String.concat ~sep:"\n"
;;

let summarise_with ~sleep ~request ~relevant_items =
  let run items = retry_request ~sleep ~request:(fun () -> request items) in
  match run relevant_items with
  | summary -> Ok summary
  | exception Retries_exhausted whole_history_cause ->
    let shared, compactable = List.partition_tf relevant_items ~f:is_shared_context in
    let groups = grouped_items compactable in
    (match List.length groups with
     | 0 | 1 -> Error whole_history_cause
     | group_count ->
       let first_groups, second_groups = List.split_n groups ((group_count + 1) / 2) in
       let first_items = List.concat [ shared; List.concat first_groups ] in
       (match run first_items with
        | first_result ->
          let second_items =
            List.concat
              [ shared; [ rolling_summary_item first_result ]; List.concat second_groups ]
          in
          (match run second_items with
           | second_result -> Ok (label_results [ first_result; second_result ])
           | exception Retries_exhausted cause -> Error cause
           | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
           | exception exn -> Error exn)
        | exception Retries_exhausted cause -> Error cause
        | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
        | exception exn -> Error exn))
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn -> Error exn
;;

let find_text output =
  let open Openai.Responses in
  let rec loop = function
    | [] -> None
    | Item.Output_message om :: rest ->
      let text = string_of_output_content om.Output_message.content in
      if String.is_empty text then loop rest else Some text
    | _ :: rest -> loop rest
  in
  loop output
;;

let request_summary ~env items =
  let open Openai.Responses in
  let transcript = render_transcript items in
  let dir = Eio.Stdenv.fs env in
  let net = Eio.Stdenv.net env in
  let text_item text : Input_message.content_item = Text { text; _type = "input_text" } in
  let mk_input role text : Item.t =
    Item.Input_message { role; content = [ text_item text ]; _type = "message" }
  in
  let inputs =
    [ mk_input Developer prompt
    ; mk_input User (sprintf "<conversation>%s</conversation>" transcript)
    ]
  in
  let reasoning = Request.Reasoning.{ effort = None; summary = None } in
  let response =
    Eio.Switch.run (fun sw ->
      post_response
        Default
        ~sw
        ~max_output_tokens:100000
        ~model:(Request.Unknown "gpt-5.6-sol")
        ~dir
        ~reasoning
        net
        ~inputs)
  in
  match find_text response.Response.output with
  | Some text -> text
  | None -> raise Missing_summary
;;

let summarise
      ~(relevant_items : Openai.Responses.Item.t list)
      ~(env : Eio_unix.Stdenv.base option)
  : (string, exn) Result.t
  =
  Log.emit `Info
  @@ sprintf "Summarizer.summarise: %d relevant items" (List.length relevant_items);
  let transcript = render_transcript relevant_items in
  let api_key_present = Option.is_some (Sys.getenv "OPENAI_API_KEY") in
  match env, api_key_present with
  | None, _ | _, false -> Ok (String.prefix transcript max_stub_chars)
  | Some env, true ->
    let dir = Eio.Stdenv.fs env in
    let result =
      summarise_with
        ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock env))
        ~request:(request_summary ~env)
        ~relevant_items
    in
    Result.iter_error result ~f:(fun exn ->
      eprintf "Summarizer.summarise: %s\n%!" (Exn.to_string exn);
      Io.log ~dir ~file:"Summarizer.summarise.error-log.txt" (Exn.to_string exn));
    result
;;

module For_testing = struct
  let render_transcript = render_transcript
  let summarise_with = summarise_with
end
