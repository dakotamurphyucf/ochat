open! Core
module Item = Openai.Responses.Item

let history_id entry =
  Printf.sprintf
    " ochat-history-id=%S"
    (History_entry.id entry |> History_entry.Id.to_string)
;;

let output_string = function
  | Openai.Responses.Tool_output.Output.Text text -> text
  | Content content ->
    List.map content ~f:(function
      | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
      | Input_image { image_url; _ } -> Printf.sprintf "<img src=%S />" image_url)
    |> String.concat ~sep:"\n"
;;

let input_message entry (message : Openai.Responses.Input_message.t) =
  let role = Openai.Responses.Input_message.role_to_string message.role in
  let content =
    List.map message.content ~f:(function
      | Openai.Responses.Input_message.Text { text; _ } ->
        Printf.sprintf "RAW|\n%s\n|RAW" text
      | Image { image_url; _ } -> Printf.sprintf "<img src=%S />" image_url)
    |> String.concat
  in
  Printf.sprintf "<msg role=%S%s>\n%s\n</msg>\n" role (history_id entry) content
;;

let output_message entry (message : Openai.Responses.Output_message.t) =
  let content =
    List.map message.content ~f:(fun item -> item.text) |> String.concat ~sep:" "
  in
  Printf.sprintf
    "<assistant id=%S status=%S%s>\nRAW|\n%s\n|RAW\n</assistant>\n"
    message.id
    message.status
    (history_id entry)
    content
;;

let function_call entry (call : Openai.Responses.Function_call.t) =
  Printf.sprintf
    "<tool_call function_name=%S tool_call_id=%S%s%s>\nRAW|\n%s\n|RAW\n</tool_call>\n"
    call.name
    call.call_id
    (Option.value_map call.id ~default:"" ~f:(Printf.sprintf " id=%S"))
    (history_id entry)
    call.arguments
;;

let custom_tool_call entry (call : Openai.Responses.Custom_tool_call.t) =
  Printf.sprintf
    "<tool_call type=\"custom_tool_call\" function_name=%S tool_call_id=%S%s%s>\n\
     RAW|\n\
     %s\n\
     |RAW\n\
     </tool_call>\n"
    call.name
    call.call_id
    (Option.value_map call.id ~default:"" ~f:(Printf.sprintf " id=%S"))
    (history_id entry)
    call.input
;;

let function_output entry (output : Openai.Responses.Function_call_output.t) =
  Printf.sprintf
    "<tool_response tool_call_id=%S%s>\nRAW|\n%s\n|RAW\n</tool_response>\n"
    output.call_id
    (history_id entry)
    (output_string output.output)
;;

let custom_tool_output entry (output : Openai.Responses.Custom_tool_call_output.t) =
  Printf.sprintf
    "<tool_response type=\"custom_tool_call\" tool_call_id=%S%s>\n\
     RAW|\n\
     %s\n\
     |RAW\n\
     </tool_response>\n"
    output.call_id
    (history_id entry)
    (output_string output.output)
;;

let reasoning entry (reasoning : Openai.Responses.Reasoning.t) =
  let summaries =
    List.map reasoning.summary ~f:(fun summary ->
      Printf.sprintf
        "<summary type=%S>RAW|\n%s\n|RAW</summary>"
        summary._type
        summary.text)
    |> String.concat
  in
  Printf.sprintf
    "<reasoning id=%S%s%s>%s</reasoning>\n"
    reasoning.id
    (Option.value_map reasoning.status ~default:"" ~f:(Printf.sprintf " status=%S"))
    (history_id entry)
    summaries
;;

let search_call entry kind id =
  Printf.sprintf "<msg role=\"assistant\" id=%S%s>%s</msg>\n" id (history_id entry) kind
;;

let render_entry entry =
  match History_entry.item entry with
  | Item.Input_message message -> input_message entry message
  | Output_message message -> output_message entry message
  | Function_call call -> function_call entry call
  | Custom_tool_call call -> custom_tool_call entry call
  | Function_call_output output -> function_output entry output
  | Custom_tool_call_output output -> custom_tool_output entry output
  | Reasoning value -> reasoning entry value
  | Web_search_call call -> search_call entry "web_search" call.id
  | File_search_call call -> search_call entry "file_search" call.id
;;

let render entries = List.map entries ~f:render_entry |> String.concat

let render_protocol entries =
  List.map entries ~f:(fun entry ->
    let open Result.Let_syntax in
    let%map history = History_codec.of_protocol entry in
    let annotation =
      match entry.Agent_protocol.History.provenance with
      | Runtime_notification id ->
        Printf.sprintf
          "<!-- ochat-runtime-notification delivery_id=%S -->\n"
          (Agent_protocol.Id.Delivery.to_string id)
      | Canonical | Moderator_inserted | Moderator_replaced _ -> ""
    in
    annotation ^ render_entry history)
  |> Result.all
  |> Result.map ~f:String.concat
;;
