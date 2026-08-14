open Core
module Fetch = Chat_response.Fetch
module Value_codec = Chatml.Chatml_value_codec
module Res_item = Openai.Responses.Item
module Moderator = Session.Moderator_snapshot

let write_user_message ~dir ~file message =
  let xml = Io.load_doc ~dir file in
  let xml = String.rstrip xml in
  let user_open = "<user>" in
  let user_close = "</user>" in
  let new_msg = Printf.sprintf "%s\n%s\n%s\n" user_open message user_close in
  let updated_xml =
    if String.is_suffix xml ~suffix:(user_open ^ "\n\n" ^ user_close)
    then (
      let base =
        String.drop_suffix xml (String.length user_open + String.length user_close + 2)
      in
      base ^ new_msg)
    else xml ^ "\n" ^ new_msg
  in
  Io.save_doc ~dir file updated_xml
;;

let to_persisted_string = function
  | Openai.Responses.Tool_output.Output.Text text -> text
  | Content cont ->
    List.map cont ~f:(fun part ->
      match part with
      | Input_text { text } -> text
      | Input_image { image_url; _ } -> Printf.sprintf "<img src=\"%s\" />" image_url)
    |> String.concat ~sep:"\n"
;;

let generic_msg_as_chatmd ?id ~role content =
  let id_attr =
    match id with
    | None -> ""
    | Some id -> Printf.sprintf " id=%S" id
  in
  Printf.sprintf "<msg role=%S%s>\n%s\n</msg>\n" role id_attr content
;;

let history_id_attribute id =
  Printf.sprintf " ochat-history-id=%S" (History_entry.Id.to_string id)
;;

let history_entry_as_chatmd entry =
  let history_id = history_id_attribute (History_entry.id entry) in
  match History_entry.item entry with
  | Res_item.Input_message message ->
    let role = Openai.Responses.Input_message.role_to_string message.role in
    let content =
      List.map message.content ~f:(function
        | Openai.Responses.Input_message.Text { text; _ } ->
          Printf.sprintf "RAW|\n%s\n|RAW" text
        | Image { image_url; _ } -> Printf.sprintf "<img src=%S />" image_url)
      |> String.concat ~sep:""
    in
    Printf.sprintf "<msg role=%S%s>\n%s\n</msg>\n" role history_id content
  | Res_item.Output_message message ->
    let content =
      List.map message.content ~f:(fun content -> content.text) |> String.concat ~sep:" "
    in
    Printf.sprintf
      "<assistant id=%S status=%S%s>\nRAW|\n%s\n|RAW\n</assistant>\n"
      message.id
      message.status
      history_id
      content
  | Res_item.Function_call call ->
    Printf.sprintf
      "<tool_call function_name=%S tool_call_id=%S%s%s>\nRAW|\n%s\n|RAW\n</tool_call>\n"
      call.name
      call.call_id
      (Option.value_map call.id ~default:"" ~f:(Printf.sprintf " id=%S"))
      history_id
      call.arguments
  | Res_item.Custom_tool_call call ->
    Printf.sprintf
      "<tool_call type=\"custom_tool_call\" function_name=%S tool_call_id=%S%s%s>\n\
       RAW|\n\
       %s\n\
       |RAW\n\
       </tool_call>\n"
      call.name
      call.call_id
      (Option.value_map call.id ~default:"" ~f:(Printf.sprintf " id=%S"))
      history_id
      call.input
  | Res_item.Function_call_output output ->
    Printf.sprintf
      "<tool_response tool_call_id=%S%s>\nRAW|\n%s\n|RAW\n</tool_response>\n"
      output.call_id
      history_id
      (to_persisted_string output.output)
  | Res_item.Custom_tool_call_output output ->
    Printf.sprintf
      "<tool_response type=\"custom_tool_call\" tool_call_id=%S%s>\n\
       RAW|\n\
       %s\n\
       |RAW\n\
       </tool_response>\n"
      output.call_id
      history_id
      (to_persisted_string output.output)
  | Res_item.Reasoning reasoning ->
    let summaries =
      List.map reasoning.summary ~f:(fun summary ->
        Printf.sprintf
          "<summary type=%S>RAW|\n%s\n|RAW</summary>"
          summary._type
          summary.text)
      |> String.concat ~sep:""
    in
    Printf.sprintf
      "<reasoning id=%S%s%s>%s</reasoning>\n"
      reasoning.id
      (Option.value_map reasoning.status ~default:"" ~f:(Printf.sprintf " status=%S"))
      history_id
      summaries
  | Res_item.Web_search_call call ->
    Printf.sprintf "<msg role=\"assistant\" id=%S%s>web_search</msg>\n" call.id history_id
  | Res_item.File_search_call call ->
    Printf.sprintf
      "<msg role=\"assistant\" id=%S%s>file_search</msg>\n"
      call.id
      history_id
;;

let canonical_entries_as_chatmd history =
  List.map history ~f:history_entry_as_chatmd |> String.concat ~sep:""
;;

module Checkpoint = struct
  type t = string Hashtbl.M(History_entry.Id).t

  let empty () = Hashtbl.create (module History_entry.Id)

  let fingerprint entry =
    History_entry.item entry |> Openai.Responses.Item.sexp_of_t |> Sexp.to_string_mach
  ;;

  let of_entries entries =
    let checkpoint = empty () in
    List.iter entries ~f:(fun entry ->
      Hashtbl.set checkpoint ~key:(History_entry.id entry) ~data:(fingerprint entry));
    checkpoint
  ;;

  let contains_unchanged t entry =
    Hashtbl.find t (History_entry.id entry)
    |> Option.exists ~f:(String.equal (fingerprint entry))
  ;;
end

let entries_after_checkpoint checkpoint history =
  List.filter history ~f:(fun entry ->
    not (Checkpoint.contains_unchanged checkpoint entry))
;;

let response_item_of_moderator_item (item : Moderator.Item.t) : Res_item.t option =
  let open Result.Let_syntax in
  let result =
    let%bind value = Value_codec.Snapshot.to_value item.value in
    match Value_codec.value_to_jsonaf_result value with
    | Error msg -> Error msg
    | Ok json ->
      (try Ok (Res_item.t_of_jsonaf json) with
       | exn -> Error (Exn.to_string exn))
  in
  Result.ok result
;;

let chatmd_of_response_item (item : Res_item.t) : string =
  match item with
  | Res_item.Input_message im ->
    let role = Openai.Responses.Input_message.role_to_string im.role in
    let content =
      List.filter_map im.content ~f:(function
        | Openai.Responses.Input_message.Text { text; _ } -> Some text
        | _ -> None)
      |> String.concat ~sep:""
    in
    (match role with
     | "user" -> Printf.sprintf "<user>\n%s\n</user>\n" content
     | "assistant" -> Printf.sprintf "<assistant>\n%s\n</assistant>\n" content
     | "tool" -> Printf.sprintf "<tool_response>\n%s\n</tool_response>\n" content
     | _ -> generic_msg_as_chatmd ~role content)
  | Res_item.Output_message om ->
    let text = List.map om.content ~f:(fun c -> c.text) |> String.concat ~sep:" " in
    generic_msg_as_chatmd ~id:om.id ~role:"assistant" text
  | Res_item.Function_call fc ->
    generic_msg_as_chatmd
      ?id:fc.id
      ~role:"tool"
      (Printf.sprintf "%s %s" fc.name fc.arguments)
  | Res_item.Custom_tool_call tc ->
    generic_msg_as_chatmd ?id:tc.id ~role:"tool" (Printf.sprintf "%s %s" tc.name tc.input)
  | Res_item.Function_call_output out ->
    generic_msg_as_chatmd ?id:out.id ~role:"tool" (to_persisted_string out.output)
  | Res_item.Custom_tool_call_output out ->
    generic_msg_as_chatmd ?id:out.id ~role:"tool" (to_persisted_string out.output)
  | Res_item.Reasoning reasoning ->
    let text =
      List.map reasoning.summary ~f:(fun summary -> summary.text)
      |> String.concat ~sep:" "
    in
    generic_msg_as_chatmd ~id:reasoning.id ~role:"assistant" text
  | Res_item.Web_search_call call ->
    generic_msg_as_chatmd ~id:call.id ~role:"assistant" "web_search"
  | Res_item.File_search_call call ->
    generic_msg_as_chatmd ~id:call.id ~role:"assistant" "file_search"
;;

let moderation_item_as_chatmd (item : Moderator.Item.t) =
  match response_item_of_moderator_item item with
  | Some response_item -> chatmd_of_response_item response_item
  | None -> generic_msg_as_chatmd ~id:item.id ~role:"developer" "Invalid moderation item"
;;

let moderation_replacement_as_chatmd (replacement : Moderator.Overlay.replacement) =
  let rendered =
    match response_item_of_moderator_item replacement.item with
    | Some response_item -> chatmd_of_response_item response_item
    | None -> "Invalid moderation item"
  in
  generic_msg_as_chatmd
    ~id:(Printf.sprintf "moderation-replacement-%s" replacement.target_id)
    ~role:"developer"
    (Printf.sprintf "Moderator replaced item %S with:\n%s" replacement.target_id rendered)
;;

let moderation_deletion_as_chatmd deleted_message_id =
  let content =
    Printf.sprintf
      "Moderator deleted message %S from the effective transcript."
      deleted_message_id
  in
  generic_msg_as_chatmd
    ~id:(Printf.sprintf "moderation-deletion-%s" deleted_message_id)
    ~role:"developer"
    content
;;

let moderation_halt_as_chatmd reason =
  generic_msg_as_chatmd
    ~id:"moderation-halt"
    ~role:"developer"
    (Printf.sprintf "Session ended by moderator: %s" reason)
;;

let overlay_as_chatmd (overlay : Moderator.Overlay.t) =
  let prepended = List.map overlay.prepended_system_items ~f:moderation_item_as_chatmd in
  let appended = List.map overlay.appended_items ~f:moderation_item_as_chatmd in
  let replacements = List.map overlay.replacements ~f:moderation_replacement_as_chatmd in
  let deletions = List.map overlay.deleted_item_ids ~f:moderation_deletion_as_chatmd in
  let halt =
    Option.to_list (Option.map overlay.halted_reason ~f:moderation_halt_as_chatmd)
  in
  String.concat ~sep:"" (prepended @ appended @ replacements @ deletions @ halt)
;;

let history_entries_as_chatmd ~moderator_snapshot ~history =
  let canonical = canonical_entries_as_chatmd history in
  let overlay =
    Option.value_map moderator_snapshot ~default:"" ~f:(fun snapshot ->
      overlay_as_chatmd snapshot.Moderator.overlay)
  in
  canonical ^ overlay
;;

let persist_entries
      ~dir
      ~prompt_file
      ~(checkpoint : Checkpoint.t)
      ~moderator_snapshot
      ~history
  =
  let suffix = entries_after_checkpoint checkpoint history in
  let rendered = history_entries_as_chatmd ~moderator_snapshot ~history:suffix in
  let existing = Io.load_doc ~dir prompt_file |> String.rstrip in
  Io.save_doc ~dir prompt_file (existing ^ "\n" ^ rendered)
;;
