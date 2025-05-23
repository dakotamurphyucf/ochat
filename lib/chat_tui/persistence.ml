open Core
module Fetch = Chat_response.Fetch
module Res_item = Openai.Responses.Item
module Config = Chat_response.Config

let write_user_message ~dir ~file message =
  let xml = Io.load_doc ~dir file in
  let xml = String.rstrip xml in
  let user_open = "<msg role=\"user\">" in
  let user_close = "</msg>" in
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

let persist_session
      ~(dir : _ Eio.Path.t)
      ~(prompt_file : string)
      ~(datadir : _ Eio.Path.t)
      ~(cfg : Config.t)
      ~(initial_msg_count : int)
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
      append (Printf.sprintf "<msg role=\"%s\">\n%s\n</msg>\n" role content)
    | Res_item.Output_message om ->
      let text = List.map om.content ~f:(fun c -> c.text) |> String.concat ~sep:" " in
      append
        (Printf.sprintf
           "\n<msg role=\"assistant\" id=\"%s\">\nRAW|\n%s\n|RAW\n</msg>\n"
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
              <msg role=\"tool\" tool_call tool_call_id=\"%s\" function_name=\"%s\" \
              id=\"%s\">\n\
              %s|\n\
              %s\n\
              |%s\n\
              </msg>\n"
             fc.name
             fc.call_id
             (Option.value fc.id ~default:fc.call_id)
             "RAW"
             fc.arguments
             "RAW")
      else (
        let filename = Printf.sprintf "%i.tool-call.%s.json" idx fc.call_id in
        Io.save_doc ~dir:datadir filename fc.arguments;
        append
          (Printf.sprintf
             "<msg role=\"tool\" tool_call function_name=\"%s\" tool_call_id=\"%s\" \
              id=\"%s\"><doc src=\"./.chatmd/%s\" local/></msg>\n"
             fc.name
             fc.call_id
             (Option.value fc.id ~default:fc.call_id)
             filename))
    | Res_item.Function_call_output fco ->
      (match Hashtbl.find tool_call_index_by_id fco.call_id with
       | None -> ()
       | Some idx ->
         if cfg.show_tool_call
         then
           append
             (Printf.sprintf
                "<msg role=\"tool\" tool_call_id=\"%s\">\nRAW|\n%s\n|RAW\n</msg>\n"
                fco.call_id
                fco.output)
         else (
           let filename = Printf.sprintf "%i.tool-call-result.%s.json" idx fco.call_id in
           Io.save_doc ~dir:datadir filename fco.output;
           append
             (Printf.sprintf
                "<msg role=\"tool\" tool_call_id=\"%s\"><doc src=\"./.chatmd/%s\" \
                 local/></msg>\n"
                fco.call_id
                filename)))
    | Res_item.Reasoning r ->
      let summaries =
        List.map r.summary ~f:(fun s ->
          Printf.sprintf "\n<summary>\n%s\n</summary>\n" s.text)
        |> String.concat ~sep:""
      in
      append (Printf.sprintf "\n<reasoning id=\"%s\">%s\n</reasoning>\n" r.id summaries)
    | _ -> ());
  Io.append_doc ~dir prompt_file (Buffer.contents buf)
;;
