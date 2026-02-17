open Core
open Eio.Std
module Res = Openai.Responses
module Req = Res.Request

let cursor_marker = "⟦CURSOR⟧"
let max_draft_before_cursor = 4000
let max_draft_after_cursor = 4000
let max_returned_chars = 4_000

let insert_marker ~text ~cursor =
  let cursor = Int.min (String.length text) (Int.max 0 cursor) in
  String.prefix text cursor ^ cursor_marker ^ String.drop_prefix text cursor
;;

let excerpt_draft ~draft ~cursor =
  let len = String.length draft in
  let cursor = Int.min len (Int.max 0 cursor) in
  if len <= max_draft_before_cursor + max_draft_after_cursor
  then insert_marker ~text:draft ~cursor
  else (
    let start = Int.max 0 (cursor - max_draft_before_cursor) in
    let stop = Int.min len (cursor + max_draft_after_cursor) in
    let sub = String.sub draft ~pos:start ~len:(stop - start) in
    let cursor_in_sub = cursor - start in
    let prefix = if start = 0 then "" else "…\n" in
    let suffix = if stop = len then "" else "\n…" in
    prefix ^ insert_marker ~text:sub ~cursor:cursor_in_sub ^ suffix)
;;

let render_history_for_prompt (items : Res.Item.t list) : string =
  let render_item = function
    | Res.Item.Input_message { role; content; _type = _ } ->
      (match content with
       | Res.Input_message.Text { text; _type = _ } :: _ ->
         Some (sprintf "%s: %s" (Res.Input_message.role_to_string role) text)
       | _ -> None)
    | Res.Item.Output_message { content; _ } ->
      (match content with
       | { text; _ } :: _ -> Some (sprintf "assistant: %s" text)
       | _ -> None)
    | _ -> None
  in
  items |> List.filter_map ~f:render_item |> String.concat ~sep:"\n"
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
- a conversation context
- a draft buffer excerpt containing the literal marker ⟦CURSOR⟧ that indicates the insertion point

Return ONLY the text to insert at ⟦CURSOR⟧ (the suffix after the marker).

Guidelines for Completion:
- Carefully review all supplementary context within one or more context blocks.
- Use what you learn from context for more accurate predictions, *never* for referencing or summarizing; the context should inform completions, not appear in them.
- Predict the most suitable code or text completion
- Prefer succinct, single-line completions by default, but produce multi-line/block completions if and only if the structure of the input clearly signals a multi-line or block output.

Constraints:
- Output must be the insertion text only (no quotes, no explanations, no Markdown fences).
- Do not repeat any draft text that appears before ⟦CURSOR⟧.
- Keep it short; if no completion is appropriate, return an empty string.
- If the input position clearly requests block-level or multi-line completion, provide it accordingly.
- Do not use tools.
|}
;;

let complete_suffix
      ~sw
      ~env
      ~dir
      ~(cfg : Chat_response.Config.t)
      ~history_items
      ~draft
      ~cursor
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
    let temperature = Option.value cfg.temperature ~default:0.2 in
    let max_output_tokens =
      let cfg_max = Option.value cfg.max_tokens ~default:500 in
      Int.min 500 (Int.max 1 cfg_max)
    in
    let model = Option.map cfg.model ~f:Req.model_of_str_exn in
    let reasoning = Req.Reasoning.{ effort = Some None; summary = None } in
    let conversation = render_history_for_prompt history_items in
    let draft_excerpt = excerpt_draft ~draft ~cursor in
    let user_prompt =
      String.concat
        ~sep:"\n\n"
        [ "<conversation>"
        ; conversation
        ; "</conversation>"
        ; "<draft>"
        ; draft_excerpt
        ; "</draft>"
        ; sprintf "cursor_index: %d" cursor
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
        ~max_output_tokens
        ~temperature
        ~verbosity:"low"
        ~tools:[]
        ?model
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
    |> strip_code_fences
    |> fun s ->
    String.substr_replace_all s ~pattern:cursor_marker ~with_:""
    |> fun s -> String.prefix s max_returned_chars |> Util.sanitize ~strip:false)
;;
