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

type projection =
  { rows : Projected_message.t list
  ; index_by_id : (Projected_message.Id.t, int) Hashtbl.t
  }

let create_projection rows =
  let index_by_id = Hashtbl.create (module Projected_message.Id) in
  List.iteri rows ~f:(fun index row ->
    Hashtbl.add_exn index_by_id ~key:row.Projected_message.id ~data:index);
  { rows; index_by_id }
;;

let canonical_row entry =
  Option.map
    (pair_of_item (History_entry.item entry))
    ~f:(fun message ->
      let entry_id = History_entry.id entry in
      Projected_message.canonical_row ~entry_id message)
;;

let project_entries entries =
  List.filter_map entries ~f:canonical_row |> create_projection
;;

let project_effective_entry
      ({ entry; provenance } : Chat_response.Moderation.Effective_entry.t)
  =
  Option.map
    (pair_of_item (History_entry.item entry))
    ~f:(fun message ->
      let entry_id = History_entry.id entry in
      match provenance with
      | Canonical ->
        Projected_message.
          { id = Id.canonical entry_id
          ; entry_id = Some entry_id
          ; message
          ; provenance = Canonical
          ; source = Canonical { entry_id }
          ; revision = 0
          }
      | Moderator_inserted { change_id } ->
        Projected_message.
          { id = Id.canonical entry_id
          ; entry_id = Some entry_id
          ; message
          ; provenance = Moderator_inserted { change_id }
          ; source = Moderator_inserted { entry_id; change_id }
          ; revision = 0
          }
      | Moderator_replacement { target_id; change_id } ->
        Projected_message.
          { id = Id.canonical target_id
          ; entry_id = Some target_id
          ; message
          ; provenance = Moderator_replacement { target_id; change_id }
          ; source = Moderator_replacement { target_id; change_id }
          ; revision = 0
          })
;;

let project_effective_entries entries =
  List.filter_map entries ~f:project_effective_entry |> create_projection
;;

let rows t = t.rows
let messages t = List.map t.rows ~f:(fun row -> row.Projected_message.message)
let index_of_id t id = Hashtbl.find t.index_by_id id

let append_local t ~id ~message ~provenance ~source =
  let row =
    Projected_message.{ id; entry_id = None; message; provenance; source; revision = 0 }
  in
  create_projection (t.rows @ [ row ])
;;

let append_pending_approval t ~local_id ~text =
  Result.map
    (Projected_message.Id.local ~namespace:"pending-approval" ~local_id)
    ~f:(fun id ->
      append_local
        t
        ~id
        ~message:("system", text)
        ~provenance:Projected_message.Pending_approval
        ~source:(Pending_approval { local_id }))
;;

let append_placeholder t ~local_id ~kind message =
  Result.map (Projected_message.Id.local ~namespace:"placeholder" ~local_id) ~f:(fun id ->
    append_local
      t
      ~id
      ~message
      ~provenance:Projected_message.Placeholder
      ~source:(Placeholder { local_id; kind }))
;;
