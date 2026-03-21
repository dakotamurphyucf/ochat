open Core
module Fetch = Chat_response.Fetch
module Res_item = Openai.Responses.Item
module Config = Chat_response.Config
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

let moderation_message_as_chatmd (message : Moderator.Message.t) =
  generic_msg_as_chatmd ~id:message.id ~role:message.role message.content
;;

let moderation_replacement_as_chatmd (replacement : Moderator.Overlay.replacement) =
  let content =
    Printf.sprintf
      "Moderator replaced message %S with a synthetic %s message:\n%s"
      replacement.target_id
      replacement.message.role
      replacement.message.content
  in
  generic_msg_as_chatmd
    ~id:(Printf.sprintf "moderation-replacement-%s" replacement.target_id)
    ~role:"developer"
    content
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
    ~role:"system"
    (Printf.sprintf "Session ended by moderator: %s" reason)
;;

let overlay_as_chatmd (overlay : Moderator.Overlay.t) =
  let prepended =
    List.map overlay.prepended_system_messages ~f:moderation_message_as_chatmd
  in
  let appended = List.map overlay.appended_messages ~f:moderation_message_as_chatmd in
  let replacements = List.map overlay.replacements ~f:moderation_replacement_as_chatmd in
  let deletions = List.map overlay.deleted_message_ids ~f:moderation_deletion_as_chatmd in
  let halt =
    Option.to_list (Option.map overlay.halted_reason ~f:moderation_halt_as_chatmd)
  in
  String.concat ~sep:"" (prepended @ appended @ replacements @ deletions @ halt)
;;

let history_as_chatmd
      ~(moderator_snapshot : Moderator.t option)
      ~(history_items : Res_item.t list)
  =
  let buf = Buffer.create 4096 in
  let append s = Buffer.add_string buf s in
  let fn_id = ref 0 in
  let tool_call_index_by_id = Hashtbl.create (module String) in
  List.iter history_items ~f:(function
    | Res_item.Input_message im ->
      let role = Openai.Responses.Input_message.role_to_string im.role in
      let content =
        List.filter_map im.content ~f:(function
          | Openai.Responses.Input_message.Text { text; _ } -> Some text
          | _ -> None)
        |> String.concat ~sep:""
      in
      (match role with
       | "user" -> append (Printf.sprintf "<user>\n%s\n</user>\n" content)
       | "assistant" -> append (Printf.sprintf "<assistant>\n%s\n</assistant>\n" content)
       | "tool" ->
         append (Printf.sprintf "<tool_response>\n%s\n</tool_response>\n" content)
       | _ -> append (generic_msg_as_chatmd ~role content))
    | Res_item.Output_message om ->
      let text = List.map om.content ~f:(fun c -> c.text) |> String.concat ~sep:" " in
      append
        (Printf.sprintf
           "\n<assistant id=\"%s\">\nRAW|\n%s\n|RAW\n</assistant>\n"
           om.id
           text)
    | Res_item.Function_call fc ->
      let idx =
        match Hashtbl.find tool_call_index_by_id fc.call_id with
        | Some i -> i
        | None ->
          let i = !fn_id in
          Hashtbl.set tool_call_index_by_id ~key:fc.call_id ~data:i;
          Int.incr fn_id;
          i
      in
      append
        (Printf.sprintf
           "<tool_call function_name=\"%s\" tool_call_id=\"%s\" id=\"%s\"><doc \
            src=\"./.chatmd/%i.tool-call.%s.json\" local/></tool_call>\n"
           fc.name
           fc.call_id
           (Option.value fc.id ~default:fc.call_id)
           idx
           fc.call_id)
    | Res_item.Custom_tool_call tc ->
      let idx =
        match Hashtbl.find tool_call_index_by_id tc.call_id with
        | Some i -> i
        | None ->
          let i = !fn_id in
          Hashtbl.set tool_call_index_by_id ~key:tc.call_id ~data:i;
          Int.incr fn_id;
          i
      in
      append
        (Printf.sprintf
           "<tool_call type=\"custom_tool_call\" function_name=\"%s\" \
            tool_call_id=\"%s\" id=\"%s\"><doc src=\"./.chatmd/%i.tool-call.%s.json\" \
            local/></tool_call>\n"
           tc.name
           tc.call_id
           (Option.value tc.id ~default:tc.call_id)
           idx
           tc.call_id)
    | Res_item.Function_call_output fco ->
      (match Hashtbl.find tool_call_index_by_id fco.call_id with
       | None ->
         append
           (Printf.sprintf
              "<tool_response tool_call_id=\"%s\">\nRAW|\n%s\n|RAW\n</tool_response>\n"
              fco.call_id
              (to_persisted_string fco.output))
       | Some idx ->
         append
           (Printf.sprintf
              "<tool_response tool_call_id=\"%s\"><doc \
               src=\"./.chatmd/%i.tool-call-result.%s.json\" local/></tool_response>\n"
              fco.call_id
              idx
              fco.call_id))
    | Res_item.Custom_tool_call_output tco ->
      (match Hashtbl.find tool_call_index_by_id tco.call_id with
       | None ->
         append
           (Printf.sprintf
              "<tool_response type=\"custom_tool_call\" tool_call_id=\"%s\">\n\
               RAW|\n\
               %s\n\
               |RAW\n\
               </tool_response>\n"
              tco.call_id
              (to_persisted_string tco.output))
       | Some idx ->
         append
           (Printf.sprintf
              "<tool_response type=\"custom_tool_call\" tool_call_id=\"%s\"><doc \
               src=\"./.chatmd/%i.tool-call-result.%s.json\" local/></tool_response>\n"
              tco.call_id
              idx
              tco.call_id))
    | Res_item.Reasoning r ->
      let summaries =
        List.map r.summary ~f:(fun s ->
          Printf.sprintf "\n<summary>\n%s\n</summary>\n" s.text)
        |> String.concat ~sep:""
      in
      append (Printf.sprintf "\n<reasoning id=\"%s\">%s\n</reasoning>\n" r.id summaries)
    | _ -> ());
  Option.iter moderator_snapshot ~f:(fun (snapshot : Moderator.t) ->
    append (overlay_as_chatmd snapshot.overlay));
  Buffer.contents buf
;;

let persist_session
      ~(dir : _ Eio.Path.t)
      ~(prompt_file : string)
      ~(datadir : _ Eio.Path.t)
      ~(cfg : Config.t)
      ~(initial_msg_count : int)
      ~(moderator_snapshot : Moderator.t option)
      ~(history_items : Res_item.t list)
  =
  let buf = Buffer.create 4096 in
  let append s = Buffer.add_string buf s in
  let fn_id = ref 0 in
  let tool_call_index_by_id = Hashtbl.create (module String) in
  let new_messages = List.drop history_items initial_msg_count in
  List.iter new_messages ~f:(function
    | Res_item.Input_message im ->
      let role = Openai.Responses.Input_message.role_to_string im.role in
      let content =
        List.filter_map im.content ~f:(function
          | Openai.Responses.Input_message.Text { text; _ } -> Some text
          | _ -> None)
        |> String.concat ~sep:""
      in
      (match role with
       | "user" -> append (Printf.sprintf "<user>\n%s\n</user>\n" content)
       | "assistant" -> append (Printf.sprintf "<assistant>\n%s\n</assistant>\n" content)
       | "tool" ->
         append (Printf.sprintf "<tool_response>\n%s\n</tool_response>\n" content)
       | _ -> append (Printf.sprintf "<msg role=\"%s\">\n%s\n</msg>\n" role content))
    | Res_item.Output_message om ->
      let text = List.map om.content ~f:(fun c -> c.text) |> String.concat ~sep:" " in
      append
        (Printf.sprintf
           "\n<assistant id=\"%s\">\nRAW|\n%s\n|RAW\n</assistant>\n"
           om.id
           text)
    | Res_item.Function_call fc ->
      let idx =
        match Hashtbl.find tool_call_index_by_id fc.call_id with
        | Some i -> i
        | None ->
          let i = !fn_id in
          Hashtbl.set tool_call_index_by_id ~key:fc.call_id ~data:i;
          Int.incr fn_id;
          i
      in
      if cfg.show_tool_call
      then
        append
          (Printf.sprintf
             "\n\
              <tool_call tool_call_id=\"%s\" function_name=\"%s\" id=\"%s\">\n\
              %s|\n\
              %s\n\
              |%s\n\
              </tool_call>\n"
             fc.call_id
             fc.name
             (Option.value fc.id ~default:fc.call_id)
             "RAW"
             fc.arguments
             "RAW")
      else (
        let filename = Printf.sprintf "%i.tool-call.%s.json" idx fc.call_id in
        Io.save_doc ~dir:datadir filename fc.arguments;
        append
          (Printf.sprintf
             "<tool_call function_name=\"%s\" tool_call_id=\"%s\" id=\"%s\"><doc \
              src=\"./.chatmd/%s\" local/></tool_call>\n"
             fc.name
             fc.call_id
             (Option.value fc.id ~default:fc.call_id)
             filename))
    | Res_item.Custom_tool_call tc ->
      let idx =
        match Hashtbl.find tool_call_index_by_id tc.call_id with
        | Some i -> i
        | None ->
          let i = !fn_id in
          Hashtbl.set tool_call_index_by_id ~key:tc.call_id ~data:i;
          Int.incr fn_id;
          i
      in
      if cfg.show_tool_call
      then
        append
          (Printf.sprintf
             "\n\
              <tool_call type=\"custom_tool_call\" tool_call_id=\"%s\" \
              function_name=\"%s\" id=\"%s\">\n\
              %s|\n\
              %s\n\
              |%s\n\
              </tool_call>\n"
             tc.call_id
             tc.name
             (Option.value tc.id ~default:tc.call_id)
             "RAW"
             tc.input
             "RAW")
      else (
        let filename = Printf.sprintf "%i.tool-call.%s.json" idx tc.call_id in
        Io.save_doc ~dir:datadir filename tc.input;
        append
          (Printf.sprintf
             "<tool_call type=\"custom_tool_call\" function_name=\"%s\" \
              tool_call_id=\"%s\" id=\"%s\"><doc src=\"./.chatmd/%s\" local/></tool_call>\n"
             tc.name
             tc.call_id
             (Option.value tc.id ~default:tc.call_id)
             filename))
    | Res_item.Function_call_output fco ->
      (match Hashtbl.find tool_call_index_by_id fco.call_id with
       | None -> ()
       | Some idx ->
         if cfg.show_tool_call
         then
           append
             (Printf.sprintf
                "<tool_response tool_call_id=\"%s\">\nRAW|\n%s\n|RAW\n</tool_response>\n"
                fco.call_id
                (to_persisted_string fco.output))
         else (
           let filename = Printf.sprintf "%i.tool-call-result.%s.json" idx fco.call_id in
           Io.save_doc ~dir:datadir filename (to_persisted_string fco.output);
           append
             (Printf.sprintf
                "<tool_response tool_call_id=\"%s\"><doc src=\"./.chatmd/%s\" \
                 local/></tool_response>\n"
                fco.call_id
                filename)))
    | Res_item.Custom_tool_call_output tco ->
      (match Hashtbl.find tool_call_index_by_id tco.call_id with
       | None -> ()
       | Some idx ->
         if cfg.show_tool_call
         then
           append
             (Printf.sprintf
                "<tool_response type=\"custom_tool_call\" tool_call_id=\"%s\">\n\
                 RAW|\n\
                 %s\n\
                 |RAW\n\
                 </tool_response>\n"
                tco.call_id
                (to_persisted_string tco.output))
         else (
           let filename = Printf.sprintf "%i.tool-call-result.%s.json" idx tco.call_id in
           Io.save_doc ~dir:datadir filename (to_persisted_string tco.output);
           append
             (Printf.sprintf
                "<tool_response type=\"custom_tool_call\" tool_call_id=\"%s\"><doc \
                 src=\"./.chatmd/%s\" local/></tool_response>\n"
                tco.call_id
                filename)))
    | Res_item.Reasoning r ->
      let summaries =
        List.map r.summary ~f:(fun s ->
          Printf.sprintf "\n<summary>\n%s\n</summary>\n" s.text)
        |> String.concat ~sep:""
      in
      append (Printf.sprintf "\n<reasoning id=\"%s\">%s\n</reasoning>\n" r.id summaries)
    | _ -> ());
  Option.iter moderator_snapshot ~f:(fun (snapshot : Moderator.t) ->
    append (overlay_as_chatmd snapshot.overlay));
  Io.append_doc ~dir prompt_file (Buffer.contents buf)
;;
