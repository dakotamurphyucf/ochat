open Core
open Types
module Util = Util
module Res_item = Openai.Responses.Item

(* Convert a single OpenAI response item into a `(role * text)` tuple that
   is consumable by the renderer.  Large payloads are sanitised and
   truncated so the TUI cannot be disrupted by control characters or
   excessively long lines. *)

let pair_of_item (it : Res_item.t) : message option =
  let sanitize = Util.sanitize ~strip:true in
  let string_of_tool_output (out : Openai.Responses.Tool_output.Output.t) : string =
    match out with
    | Openai.Responses.Tool_output.Output.Text text -> text
    | Content parts ->
      parts
      |> List.map ~f:(function
        | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
        | Input_image { image_url; _ } -> Printf.sprintf "<image src=\"%s\" />" image_url)
      |> String.concat ~sep:"\n"
  in
  let string_of_content_items content_items =
    List.filter_map content_items ~f:(function
      | Openai.Responses.Input_message.Text { text; _ } -> Some (sanitize text)
      | _ -> None)
    |> String.concat ~sep:"\n"
  in
  match it with
  | Res_item.Input_message im ->
    let role =
      Openai.Responses.Input_message.role_to_string im.role |> String.lowercase
    in
    let text = string_of_content_items im.content in
    Some (role, text)
  | Res_item.Output_message om ->
    let role = "assistant" in
    let text =
      List.map om.content ~f:(fun c -> Util.sanitize ~strip:false c.text)
      |> String.concat ~sep:" "
    in
    Some (role, text)
  | Res_item.Function_call fc ->
    let role = "tool" in
    Some (role, Printf.sprintf "%s(%s)" fc.name (sanitize fc.arguments))
  | Res_item.Custom_tool_call tc ->
    let role = "tool" in
    Some (role, Printf.sprintf "%s(%s)" tc.name (sanitize tc.input))
  | Res_item.Function_call_output fco ->
    let role = "tool_output" in
    let max_len = 10_000 in
    let output = string_of_tool_output fco.output in
    let txt = Util.sanitize ~strip:false output in
    let txt =
      if String.length txt > max_len
      then String.sub txt ~pos:0 ~len:max_len ^ "\n…truncated…"
      else txt
    in
    Some (role, txt)
  | Res_item.Custom_tool_call_output tco ->
    let role = "tool_output" in
    let max_len = 10_000 in
    let output = string_of_tool_output tco.output in
    let txt = Util.sanitize ~strip:false output in
    let txt =
      if String.length txt > max_len
      then String.sub txt ~pos:0 ~len:max_len ^ "\n…truncated…"
      else txt
    in
    Some (role, txt)
  | Res_item.Reasoning r ->
    let role = "reasoning" in
    let txt =
      List.map r.summary ~f:(fun s -> Util.sanitize ~strip:false s.text)
      |> String.concat ~sep:" "
    in
    Some (role, txt)
  | _ -> None
;;

let of_history (items : Res_item.t list) : message list =
  List.filter_map items ~f:pair_of_item
;;
