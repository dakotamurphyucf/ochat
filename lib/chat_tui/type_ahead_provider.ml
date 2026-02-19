open Core
open Eio.Std
module Res = Openai.Responses
module Req = Res.Request

let cursor_marker = "⟦INSERT⟧"
let max_returned_chars = 4_000
let max_message_length = 5000
let max_history_length = max_message_length * 2

let insert_marker ~text ~cursor =
  let cursor = Int.min (String.length text) (Int.max 0 cursor) in
  String.prefix text cursor ^ cursor_marker ^ String.drop_prefix text cursor
;;

let string_of_tool_output output =
  let open Openai.Responses in
  match output with
  | Tool_output.Output.Text text -> text
  | Content parts ->
    parts
    |> List.map ~f:(function
      | Tool_output.Output_part.Input_text { text } -> text
      | Input_image { image_url; _ } -> Printf.sprintf "<image src=\"%s\" />" image_url)
    |> String.concat ~sep:"\n"
;;

let render_history_for_prompt items : string =
  let render_item = function
    | "assistant", msg -> Some msg
    | "user", msg -> Some msg
    | "developer", msg -> Some msg
    | "system", msg -> Some msg
    | _ -> None
  in
  let items = List.map items ~f:(fun x -> render_item x) |> List.filter_opt in
  let len = List.length items in
  (if len > 3 then List.drop items (len - 3) else items)
  |> List.map ~f:(fun x -> String.prefix x max_message_length)
  |> String.concat ~sep:"\n"
;;

let strip_code_fences (text : string) : string =
  let lines = String.split_lines text in
  match lines with
  | first :: rest when String.is_prefix first ~prefix:"```" ->
    (match List.rev rest with
     | last :: middle_rev when String.is_prefix last ~prefix:"```" ->
       String.concat ~sep:"\n" (List.rev middle_rev)
     | _ -> text)
  | _ -> text
;;

let completion_system_prompt =
  {|
You are a type-ahead completion engine.

You are given:
- a completion context
- a draft buffer excerpt containing the literal marker ⟦INSERT⟧ that indicates the insertion point

Return ONLY the text to insert at ⟦INSERT⟧ (the suffix after the marker).

Guidelines for Completion:
- the completion context is possibly relevant information to the completion of the draft buffer.

Constraints:
- Output must be the insertion text only.
- You must ensure you do not repeat text that appears before ⟦INSERT⟧ or after it.
  <example>
  # so say you had this
  mary had a li⟦INSERT⟧

  # then you should output
  ttle lamb

  # do not output
  little lamb
  </example>
- You must ensure lines do not exceed 80 characters. Insert a newline character if necessary.
- You must keep completions short; if no completion is appropriate, return an empty string.
|}
;;

let complete_suffix ~sw ~env ~dir ~(cfg : Chat_response.Config.t) ~messages ~draft ~cursor
  =
  Switch.check sw;
  let api_key_present =
    match Sys.getenv "OPENAI_API_KEY" with
    | None -> false
    | Some key -> not (String.is_empty key)
  in
  if not api_key_present
  then ""
  else (
    let net = Eio.Stdenv.net env in
    (* let temperature = Option.value cfg.temperature ~default:0.2 in *)
    let max_output_tokens = 200 in
    let model = Req.Unknown "gpt-5-mini" in
    (* Option.map cfg.model ~f:Req.model_of_str_exn in *)
    let reasoning = Req.Reasoning.{ effort = Some Minimal; summary = None } in
    let completion_context = render_history_for_prompt messages in
    let draft_excerpt = insert_marker ~text:draft ~cursor in
    let user_prompt =
      String.concat
        ~sep:"\n\n"
        [ "<<<|completion-context-start|>>>"
        ; completion_context
        ; "<<<|completion-context-end|>>>"
        ; "<<<|draft-buffer-start|>>>"
        ; draft_excerpt
        ; "<<<|draft-buffer-end|>>>"
        ]
    in
    let text_item text : Res.Input_message.content_item =
      Res.Input_message.Text { text; _type = "input_text" }
    in
    let mk_input role text : Res.Item.t =
      let msg : Res.Input_message.t =
        { role; content = [ text_item text ]; _type = "message" }
      in
      Res.Item.Input_message msg
    in
    let inputs =
      [ mk_input Res.Input_message.System completion_system_prompt
      ; mk_input Res.Input_message.User user_prompt
      ]
    in
    let ({ Res.Response.output; _ } : Res.Response.t) =
      Res.post_response
        Res.Default
        ~max_output_tokens (* ~temperature *)
        ~verbosity:"low"
        ~tools:[]
        ~model
        ?reasoning:(Some reasoning)
        ~dir
        net
        ~inputs
    in
    let rec find_text = function
      | [] -> ""
      | Res.Item.Output_message om :: _ ->
        (match om.Res.Output_message.content with
         | { text; _ } :: _ -> text
         | _ -> "")
      | _ :: tl -> find_text tl
    in
    let raw = find_text output in
    raw
    |> fun s ->
    String.substr_replace_all s ~pattern:cursor_marker ~with_:""
    |> Util.sanitize ~strip:false)
;;
