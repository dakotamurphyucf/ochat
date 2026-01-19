open! Core

(*------------------------------------------------------------------*)
(*  Internal helpers                                                 *)
(*------------------------------------------------------------------*)

let prompt =
  {|
Your task is to create a detailed summary of the conversation so far, paying close attention to the user's explicit requests and your previous actions.
This summary should be thorough in capturing technical details, code patterns, architectural decisions, assistant outputs, and any other context that would be essential for continuing development work without losing context.

Before providing your final summary, wrap your analysis in <analysis> tags to organize your thoughts and ensure you've covered all necessary points. In your analysis process:

1. Chronologically analyze each message and section of the conversation. For each section thoroughly identify:
   - The user's explicit requests and intents
   - Your approach to addressing the user's requests and your responses
   - Key decisions, technical concepts and code patterns
   - Specific details like:
     - file names
     - full code snippets
     - function signatures
     - file edits
     - analysis outputs
  - Errors that you ran into and how you fixed them
  - Pay special attention to specific user feedback that you received, especially if the user told you to do something differently.
2. Double-check for technical accuracy and completeness, addressing each required element thoroughly.

Your Detailed summary should include the following sections:

1. Primary Request and Intent: Capture all of the user's explicit requests and intents in detail
2. Key Technical Concepts: List all important technical concepts, technologies, and frameworks discussed.
3. Files and Code Sections: Enumerate specific files and code sections examined, modified, or created. Pay special attention to the most recent messages and include full code snippets where applicable and include a summary of why this file read or edit is important.
4. Errors and fixes: List all errors that you ran into, and how you fixed them. Pay special attention to specific user feedback that you received, especially if the user told you to do something differently.
5. Problem Solving: Document problems solved and any ongoing troubleshooting efforts.
6. All user messages: List ALL user messages. These are critical for understanding the users' feedback and changing intent.
7. All relevant assistant messages: List ALL assistant messages. These are critical for understanding the assistant's responses and actions. Please ensure you are thorough and complete. If you outputted anything significant, it should be included here in detail.
8. Pending Tasks: Outline any pending tasks that you have explicitly been asked to work on.
9. Current Work: Describe in detail precisely what was being worked on immediately before this summary request, paying special attention to the most recent messages from both user and assistant. Include file names and code snippets where applicable.
10. Optional Next Step: List the next step that you will take that is related to the most recent work you were doing. IMPORTANT: ensure that this step is DIRECTLY in line with the task you were working on immediately before this summary request. If your last task was concluded, then only list next steps if they are explicitly in line with the current context such as todo list.
    if there is a next step, include direct quotes from the most recent conversation showing exactly what task you were working on and where you left off. This should be verbatim to ensure there's no drift in task interpretation.
    Include a list of commands that should be run to continue the work, and any other relevant information that would be needed to continue the work and get up to speed quickly.

Here's an example of how your output should be structured:

<example>
<analysis>
[Your thought process, ensuring all points are covered thoroughly and accurately. You must be overly detailed and precise in your analysis]
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
</example>

Please provide your Detailed summary based on the conversation so far, following this structure and ensuring precision and thoroughness in your response.
- You must ensure that your summary contains any all the relevant information needed to pick up where you left off.
- You will not be able to remember any information from this conversation after this summary is provided, so ensure that you capture all relevant information. 
- If you do not save relevant information in this summary, you will not be able to continue the conversation effectively and that is not acceptable.
- You must assume that the user will not remember any details from this conversation, so you must ensure that your summary is detailed and complete.
- Think about information that the user might want to reference in the future, and ensure that it is included in your summary.
- you should be going for 70% compression of the conversation, so you must be thorough and complete in your analysis. 
|}
;;

let max_stub_chars = 2_000

(* Render [Openai.Responses.Item.t] to a plain transcript line.  We
   intentionally restrict ourselves to the most common variants and
   ignore function-call deltas, reasoning tokens, etc.  Those are at
   best noise for the summariser. *)

let render_item (item : Openai.Responses.Item.t) : string option =
  let open Openai.Responses in
  let string_of_tool_output (output : Tool_output.Output.t) : string =
    match output with
    | Tool_output.Output.Text text -> text
    | Content parts ->
      parts
      |> List.map ~f:(function
        | Tool_output.Output_part.Input_text { text } -> text
        | Input_image { image_url; _ } ->
          Printf.sprintf "<image src=\"%s\" />" image_url)
      |> String.concat ~sep:"\n"
  in
  match item with
  | Item.Input_message { content; role; _ } ->
    (match content with
     | [] -> None
     | Text { text; _ } :: _ ->
       sprintf "%s: %s" (Input_message.role_to_string role) text |> Some
     | _ -> None)
  | Item.Output_message { content; _ } ->
    (match content with
     | [] -> None
     | { text; _ } :: _ -> sprintf "%s: %s" "Assistant" text |> Some)
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

(*------------------------------------------------------------------*)
(*  Public API                                                      *)
(*------------------------------------------------------------------*)

let summarise
      ~(relevant_items : Openai.Responses.Item.t list)
      ~(env : Eio_unix.Stdenv.base option)
  : string
  =
  Log.emit `Info
  @@ sprintf "Summarizer.summarise: %d relevant items" (List.length relevant_items);
  (* Render the transcript to a plain text string. *)
  let transcript = render_transcript relevant_items in
  (* Early-exit stub when either no environment is provided or the API
     key is missing. *)
  let api_key_present = Option.is_some (Sys.getenv "OPENAI_API_KEY") in
  match env, api_key_present with
  | None, _ | _, false -> String.prefix transcript max_stub_chars
  | Some env, true ->
    (* Build OpenAI chat completion request. *)
    let open Openai.Responses in
    let system_prompt = prompt in
    let dir = Eio.Stdenv.fs env in
    let net = Eio.Stdenv.net env in
    let open Input_message in
    let text_item text : content_item = Text { text; _type = "input_text" } in
    let mk_input role text : Item.t =
      let role =
        match role with
        | "user" -> User
        | "assistant" -> Assistant
        | "system" -> System
        | "developer" -> Developer
        | _ -> System
      in
      let msg : Input_message.t =
        { role; content = [ text_item text ]; _type = "message" }
      in
      Item.Input_message msg
    in
    let inputs =
      [ mk_input "system" system_prompt
      ; mk_input "user" (sprintf "<conversation>%s</conversation>" transcript)
      ]
    in
    (try
       let ({ Response.output; _ } : Response.t) =
         post_response
           Default
           ~max_output_tokens:100000
           ~temperature:0.3
           ~model:Request.Gpt4_1
           ~dir
           net
           ~inputs
       in
       (* Extract assistant text from first Output_message. *)
       let rec find_text = function
         | [] -> None
         | Item.Output_message om :: _ ->
           (match om.Output_message.content with
            | { text; _ } :: _ -> Some text
            | _ -> None)
         | _ :: tl -> find_text tl
       in
       match find_text output with
       | Some text -> text
       | None -> String.prefix transcript max_stub_chars
     with
     | exn ->
       eprintf "Summarizer.summarise: %s\n%!" (Exn.to_string exn);
       String.prefix transcript max_stub_chars)
;;
